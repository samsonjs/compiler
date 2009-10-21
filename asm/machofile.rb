require 'asm/macho'

module Assembler
  
  class MachOFile
    
    include MachO
    
    attr_accessor :header, :load_commands, :sections, :data
    attr_accessor :current_segment
    attr_accessor :text_offset
    
    def initialize(filetype=MH_OBJECT)
      @header = MachHeader.new(MH_MAGIC, CPU_TYPE_X86, CPU_SUBTYPE_X86_ALL, filetype, 0, 0, 0)
      @load_commands = []              # All defined segments.
      @sections = {}                   # Map of segment names to lists of segments. 
      @section_disk_size = Hash.new(0) # Sections store their VM size so we need their sizes on disk.
      @data = []                       # Blobs of data that appear at the end of the file.
                                       #   (text, data, symtab, ...)
      @current_segment = nil           # An alias for the last defined segment.
    end


    # Define a LoadCommand in this file.  The header's ncmds and sizeofcmds
    # fields are updated automatically to keep things in sync.  If a block is
    # given it is passed the new LoadCommand struct after all other
    # initialization has been done.
    #
    # Other methods that create any type of load command should use this
    # method to do so.  Right now the only types supported are LC_SEGMENT
    # and LC_SYMTAB.  Modify asm/macho.rb to add structs for other types, and
    # add them to LoadCommandStructMap.

    def load_command(cmdtype)
      struct = LoadCommandStructMap[cmdtype]
      unless struct
        raise "unsupported load command type: #{cmdtype.inspect}," + 
              " supported types: #{LoadCommandStructMap.keys.sort.inspect}"
      end
      
      # Fill in all the unknown fields with 0, this is nonsense for
      # string fields but that doesn't really matter.
      dummy_vals = [0] * (struct::Members.size - 2)
      
                         #   cmd        cmdsize          ...
      command = struct.new(cmdtype, struct.bytesize, *dummy_vals)
      
      @load_commands << command
      
      @header[:ncmds] += 1
      @header[:sizeofcmds] += command.bytesize
      
      yield(command) if block_given?
      
      return command
    end

    
    # Define a segment in this file.  If a block is given it is passed
    # the new segment.  You can chain calls to segment, it returns self.
    #
    # Mach object files should only contain one anonymous segment.  This
    # is not checked but should be kept in mind when crafting files.
    def segment(name, &block)
      @current_segment = load_command(LC_SEGMENT) do |seg|
        seg[:segname] = name
        block.call(seg) if block
      end
      return self
    end


    # Define a section under the given segment.  nsects and cmdsize are
    # updated automatically.  segname can't be derived from the segment
    # that this section is defined under, as they can differ.
    #
    # Mach object files have the __text, __data, and other common
    # sections all defined under one anonymous segment, but their segment
    # names reflect their final positions after linking.  The linker plonks
    # them in the segment that they name.
    def section(name, segname, data='', vmsize=data.size,
                segment=@current_segment, type=S_REGULAR)
      
      # Create the new section.
      section = Section.new(name, segname, 0, vmsize, 0, 0, 0, 0, 0, 0, type)
      
      # Add this section to the map of segment names to sections.
      (@sections[segment[:segname]] ||= []) << section
      @section_disk_size[name] = data.size
      @data << data if data.size > 0
      
      # Update the header.
      @header[:sizeofcmds] += section.bytesize
      
      # Update the segment.
      segment[:nsects] += 1
      segment[:cmdsize] += section.bytesize

      yield(section) if block_given?
      
      return section
    end



    # Define a standard text section under the current segment (if present).
    #
    # If there is no current segment then we act according to the file's type
    # (specified in the header).  Segments are created if they do not exist.
    #
    # When it is MH_OBJECT the text section is defined under a single,
    # nameless segment, but the section's segment name is set to the name
    # given here.
    #
    # For MH_EXECUTE files the text section goes under the segment with the
    # name given (__TEXT).
    
    def text(data, sectname='__text', segname='__TEXT')
      unless @current_segment
        segment(segname_based_on_filetype(segname)) do |seg|
          seg[:maxprot] = VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE
          seg[:initprot] = VM_PROT_READ | VM_PROT_EXECUTE
        end        
      end
      
      section(sectname, segname, data) do |sect|
        sect[:flags] = 0x400 # S_ATTR_SOME_INSTRUCTIONS
      end
      
      return self
    end


    # Basis for #data, #const, and #bss methods.
    def segment_based_on_filetype(segname, options={})
      unless @current_segment
        permissions = VM_PROT_READ
        permisions |= VM_PROT_WRITE if options.delete(:writable)
        segment(segname_based_on_filetype(segname)) do |seg|
          seg[:initprot] = seg[:maxprot] = permissions
        end
      end
      yield if block_given?
      return self
    end

    # Define a standard data section under the current segment (if present).
    # This behaves similarly to the text method.
    #
    def data(data, sectname='__data', segname='__DATA')
      segment_based_on_filetype(segname, :writable => true) do
        section(sectname, segname, data)
      end
    end

    # Define a standard const section under the current segment (if present).
    # This behaves similarly to the data method.
    #
    def const(data, sectname='__const', segname='__DATA')
      segment_based_on_filetype(segname) do
        section(sectname, segname, data)
      end
    end

    # Define a standard BSS section under the current segment (if present).
    # This behaves similarly to the data method but accepts a VM size instead
    # of a blob, and no data is written to file since this section is for
    # uninitialized data.
    #
    def bss(vmsize, sectname='__bss', segname='__DATA')
      segment_based_on_filetype(segname, :writable => true) do
        section(sectname, segname, '', vmsize)
      end
    end

   
    # Define a symbol table.  This should usually be placed at the end of the
    # file.
    #
    # This function is overloaded to accept either an array of Nlist structs
    # packed into a byte string (i.e. a C array) and a string table, or a
    # single parameter: any type of Symtab.
    
    def symtab(nlist_ary_or_symtab, stab=nil)
      if stab.nil?
        symtab = nlist_ary_or_symtab
        stab = symtab.stab
        nlist_ary = symtab.nlist_ary
      else
        nlist_ary = nlist_ary_or_symtab
      end
      
      load_command(LC_SYMTAB) do |st|
        st[:nsyms] = nlist_ary.size
        st[:strsize] = stab.size
        # symoff and stroff are filled in when offsets are recalculated.
      end
      
#       puts ">>> Defining symbol table:"
#       puts ">>> #{nlist_ary.size} symbols"
#       puts ">>> stab = #{stab.inspect}"
#       puts ">>> nlist_ary = #{nlist_ary.inspect}"
#       puts ">>> (serialized) = #{nlist_ary.map{|n|n.serialize}.join.inspect}"
      
      @data << nlist_ary.map {|n| n.serialize}.join
      @data << stab
      
      return self
    end


    # Serialize the entire MachO file into a byte string.  This is simple
    # thanks to CStruct#serialize.
    
    def serialize
      # TODO sanity checks, e.g. assert(@header[:ncmds] == @load_command.size)
      # ... perhaps an option to recalculate such data as well.
      
      # Now that we have all the pieces of the file defined we can calculate
      # the file offsets of segments and sections.
      recalculate_offsets

      
      # |------------------|
      # |  Mach Header     |          Part 1
      # |------------------|
      # |  Segment 1       |          Part 2
      # |    Section 1     | ---
      # |    Section 2     | --|--
      # |    ...           |   | |
      # |  Segment 2       |   | |
      # |    Section 4     |   | |
      # |    Section 5     |   | |
      # |    ...           |   | |
      # |  ...             |   | |
      # |  [Symtab cmd]    |   | |
      # |------------------|   | |
      # |  Section data 1  | <-- |    Part 3
      # |  Section data 2  | <----
      # |  ...             |
      # |  [Symtab data]   |
      # |------------------|      

      ###################################
      # Mach-O file Part 1: Mach Header #
      ###################################

      obj = @header.serialize

      
      #####################################
      # Mach-O file Part 2: Load Commands #
      #####################################

      # dump each load command (which include the section headers under them)
      obj += @load_commands.map do |cmd|               
               sects = @sections[cmd[:segname]] rescue []
               sects.inject(cmd.serialize) do |data, sect|
                 data + sect.serialize
               end
            end.join
      

      ###################################
      # Mach-O file Part 3: Binary data #
      ###################################

      obj += @data.join

      
      return obj
    end

    
    # Update the file offsets in segments and sections.
    
    def recalculate_offsets

      # Maintain the offset into the the file on disk.  This is used
      # to update the various structures.
      offset = @header.bytesize
            
      # First pass over load commands.  Most sizes are filled in here.
      @load_commands.each do |cmd|
        case cmd[:cmd]
          
        when LC_SEGMENT
          seg = cmd
          sections = @sections[seg[:segname]]
          section_size = sections.size * Section.bytesize
          section_vm_size = sections.inject(0) { |total, sect| total + sect[:size] }
          section_disk_size = sections.inject(0) do |total, sect|
            total + @section_disk_size[sect[:sectname]]
          end
        
          ### TODO this should be redundant. try commenting it out one day.
          seg[:nsects] = sections.size
          seg[:cmdsize] = seg.bytesize + section_size
          ###
          
          seg[:vmsize] = section_vm_size
          seg[:filesize] = section_disk_size
          
        when LC_SYMTAB
          # nop
          
        else
          raise "unsupported load command: #{cmd.inspect}"
        end

        offset += cmd[:cmdsize]
      end
      

      # offset now points to the end of the Mach-O headers, or the beginning
      # of the binary blobs of section data at the end.

      # Second pass over load commands.  Fill in file offsets.
      @load_commands.each do |cmd|
        case cmd[:cmd]\
          
        when LC_SEGMENT
          seg = cmd
          sections = @sections[seg[:segname]]        
          seg[:fileoff] = offset
          sections.each do |sect|
            sect[:offset] = offset
            offset += @section_disk_size[sect[:sectname]]
          end
          
        when LC_SYMTAB
          st = cmd
          st[:symoff] = offset
          offset += st[:nsyms] * Nlist.bytesize
          st[:stroff] = offset
          offset += st[:strsize]


        # No else clause is necessary, the first iteration should have caught them.
          
        end
        
      end # @load_commands.each
      
    end # def recalculate_offsets
  
  
    #######
    private
    #######

    def segname_based_on_filetype(segname)
      case @header[:filetype]
      when MH_OBJECT: ''
      when MH_EXECUTE: segname
      else
        raise "unsupported MachO file type! #{@header.inspect}"
      end
    end
    
    
  end # class MachOFile
  
end # module Assembler
