        
    ### XXX development hack!
    def stub_symtab!
      text_segnum = 1
      symtab_stub = {
        :functions => [
          # name     type           segnum       addr
          ['_main', N_SECT | N_EXT, text_segunm, 0x0]
        ]
      }
      
      nlist_ary = []
      stab = "\0"
      strx = 1  # string index (1-based)
      
      symtab[:functions].each do |name, type, segnum, addr|
        nlist_ary << MachO::Nlist.new(strx, type, segnum, 0, addr)
        stab << "#{name}\0"
        strx += 1
      end
      symtab(nlist_ary, stab)
    end
    
  end
  
end