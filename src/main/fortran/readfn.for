c readfn
c_________________________________________________________________NoticeStart_
c StateCU Consumptive Use Model
c StateCU is a part of Colorado's Decision Support Systems (CDSS)
c Copyright (C) 1994-2021 Colorado Department of Natural Resources
c 
c StateCU is free software:  you can redistribute it and/or modify
c     it under the terms of the GNU General Public License as published by
c     the Free Software Foundation, either version 3 of the License, or
c     (at your option) any later version.
c 
c     StateCU is distributed in the hope that it will be useful,
c     but WITHOUT ANY WARRANTY; without even the implied warranty of
c     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c     GNU General Public License for more details.
c 
c     You should have received a copy of the GNU General Public License
c     along with StateCU.  If not, see <https://www.gnu.org/licenses/>.
c_________________________________________________________________NoticeEnd___

c
c *********************************************************
c
c      Readfn; Subroutine that reads in files in the response file (*.rcu)
c              in any order to assign the correct file name variable
c
       subroutine readfn(fstring,fname,ext)
c
c               fstring = line read from rcu file
c               fname = file name without extension
c               ext = 3 (or 9) character file identifier
c
       character(len=*) fstring
       character(len=*) fname, ext
      
C####&|================================================================|jhb
C####&|  first get rid of any comments on the line                     |jhb
C####&|  look for first # symbol, eliminate it and everything after it |jhb
C####&|================================================================|jhb
         k1=0
         do k=1,len(fstring)
           if(fstring(k:k).eq.'#') then
              k1=k
              go to 1
           endif
         end do
C####&|================================================================|jhb
    1    select case (k1)
         case(0) !# not found, parse the whole line
           k1=len(fstring)
         case(1) !# found at beginning of record, skip it and go on
C       the following values SHOULD cause the select case statement in the
C       calling routine to skip the line and move on ot the next one
           ext="comment_line"
           fname=""
           return
         case default !# found, make this the end of the string to be parsed
           k1=k1-1
         end select
C####&|================================================================|jhb
C####&| find the first "=" if it exists...                             |jhb
C####&|================================================================|jhb
      j1=0
      do j=1,k1
         if(fstring(j:j).eq.'=') then
            j1=j
            go to 2
         endif
      end do
    2 if (j1.eq.0) then
C####&|  no "=" found, so                                              |jhb
C####&|  find a "," if it exists...                                    |jhb
C####&|================================================================|jhb
C####&| start existing code, but I made some changes......             |jhb
C####&|================================================================|jhb
c               Find first character ','
c _______________________________________________________ 
        do j=1,k1
          if(fstring(j:j).eq.',') then
            j1=j
            goto 10
          endif
        end do
C       did not find a = or a comma,
C       treat it like a comment line, skip the line and go on
C       the following values SHOULD cause the select case statement in the
C       calling routine to skip the line and move on ot the next one
        ext=""
        fname=""
        return
        stop
c       found a comma, going to be picky about what we find now...
10      iend=j1-1
        fname=fstring(1:iend)
c
c       Find first non blank after ','
           do k=j1+1,k1
              if(fstring(k:k).ne.' ') then
                 j2=k
                 goto 20
              endif
           end do
        write(*,*) 'missing filename after comma in response (rcu) file'
       write(999,*)'missing filename after comma in response (rcu) file'
        stop
C     now, for backward compatibility, get rid of anything after the next space...
C     this essentially allows comments on the line when a comma delimiter is used
C     without having a # - this is the old method
20    do k=j2,k1
        if(fstring(k:k).eq.' ') then
          k2=k-1
          go to 30
        end if
      end do
      write(*,*) 'missing filenam in response (rcu) file'
      write(999,*) 'missing filenam in response (rcu) file'
      goto 40
      
30    ext=fstring(j2:k2)

40    return
C####&|================================================================|jhb
C####&| ...end existing code                                           |jhb
C####&|================================================================|jhb
C
C####&|================================================================|jhb
      else
C####&|================================================================|jhb
C####&|  "=" found, so                                                 |jhb
C####&|  break the line into two pieces, assign to the variables       |jhb
C####&|================================================================|jhb
C####&|  trim spaces off beginning of token string                     |jhb
         j=1
         do while((fstring(j:j).eq.' ').and.(j.lt.j1))
            j=j+1
         end do
C####&|  trim spaces off end of token string                           |jhb
         k=j1-1
         do while((fstring(k:k).eq.' ').and.(k.gt.1))
            k=k-1
         end do
C####&|  save token string - has to be at least two characters...      |jhb
         if (k.le.j) then
            write(*,*) 'invalid file type string:', fstring(1:j1-1)
            write(999,*)  'invalid file type string:', fstring(1:j1-1)
            stop
         end if
         ext=fstring(j:k)
C####&|  trim spaces (and a double quote symbol) off beginning of file name string                 |jhb
         j=j1+1
         do while((fstring(j:j).eq.' ').and.(j.le.k1))
            j=j+1
         end do
         if (fstring(j:j).eq.'"') then
            j=j+1
         end if
C####&|  trim spaces (and a double quote) off end of file name string      |jhb
         k=k1
         do while((fstring(k:k).eq.' ').and.(k.gt.j1))
            k=k-1
         end do
C####&|  save file name string                                         |jhb
         if (fstring(k:k).eq.'"') then
            k=k-1
         end if
         if (k.le.j) then
            write(*,*) 'invalid file name string:', fstring(j1+1:k1)
            write(999,*) 'invalid file name string:', fstring(j1+1:k1)
            stop
         end if
         fname=fstring(j:k)
         return
      endif
C####&|================================================================|jhb
      stop 
      END
