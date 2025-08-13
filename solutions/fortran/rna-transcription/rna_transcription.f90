module rna_transcription
   implicit none
contains

   pure function to_rna(dna)
      character(*), intent(in) :: dna
      character(len(dna)) :: to_rna

      character :: nucleotide
      integer :: i

      do i = 1, len(dna)
         select case (dna(i:i))
          case('G')
            nucleotide = 'C'
          case('C')
            nucleotide = 'G'
          case('T')
            nucleotide = 'A'
          case('A')
            nucleotide = 'U'
          case default
            nucleotide = '?'
         end select

         to_rna(i:i) = nucleotide
      end do

   end function to_rna

end module rna_transcription
