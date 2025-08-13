module protein_translation
   implicit none
   character(len=4) :: StopCodon = "STOP"
   private
   public :: proteins
contains

   pure function proteins(rna) result(names)
      character(len=*), intent(in) :: rna
      character(len=13), allocatable :: names(:), temp(:)
      character(len=3) :: codon
      integer :: i, n, valid

      valid = 0
      n = len(rna) / 3
      allocate(temp(n))

      do i = 1,n
         codon = rna(i*3-2:i*3)
         if ( from(codon) == StopCodon ) then
            exit
         end if
         valid = valid + 1
         temp(i) = from(codon)
      end do

      if ( valid > 0 ) then
         allocate(names(valid))
         names = temp(1:valid)
      else
         names = [character(len=13) :: ]
      end if
   end function proteins

   pure function from(codon) result(name)
      character(len=*), intent(in) :: codon
      character(len=13) :: name

      select case (codon)
       case ("AUG")
         name = "Methionine"
       case ("UUU", "UUC")
         name = "Phenylalanine"
       case ("UUA","UUG")
         name = "Leucine"
       case ("UCU", "UCC", "UCA", "UCG")
         name = "Serine"
       case ("UAU", "UAC")
         name = "Tyrosine"
       case ("UGU", "UGC")
         name = "Cysteine"
       case ("UGG")
         name = "Tryptophan"
       case ("UAA", "UAG", "UGA")
         name = StopCodon
      end select
   end function from
end module protein_translation

