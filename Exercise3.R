#DNA example (creates 2 vectors)
seq_1 <- c("A","C","A","C","A","C","T","A")
seq_2 <- c("A","G","C","A","C","A","C","A")

seq_1
seq_2

#Score +1 for match, 0 for no matach
dist_nucleotides <- diag(4)
rownames(dist_nucleotides) <- c("A","C","T","G")
colnames(dist_nucleotides) <- c("A","C","T","G")
dist_identity_nucleotides <- dist_nucleotides
dist_identity_nucleotides

s <- dist_nucleotides[seq_2,seq_1]
colnames(s) <- c(seq_1)
rownames(s) <- c(seq_2)
s

#Constructing core alignment
## A Use nested loops to populate a nucleotide 
## similarity matrix where matches are +2 and 
## non-matches are -1. Name the rows and columns.

for (j in 1:ncol(dist_identity_nucleotides)){
  for (i in 1:nrow(dist_identity_nucleotides)){
    if(colnames(dist_identity_nucleotides)[j] == rownames(dist_identity_nucleotides)[i]){
      
    dist_identity_nucleotides[i,j] = 2 
    }
  else{
    dist_identity_nucleotides[i,j] = -1
    
  }
}

}
dist_identity_nucleotides

## Problem B 

s2 <- dist_identity_nucleotides[seq_2,seq_1]
colnames(s) <- c(seq_1)
rownames(s) <- c(seq_2)
s2

## Problem C -Populate an empty matrix of zeroes using the 
## dimensions of each sequence plus one additional cell to 
## pad the upper-left corner, as shown.

# Creates empty matrix with one extra row and column
mat1 <- matrix(data = 0, nrow = (length(seq_1) +1), ncol = (length(seq_2) + 1))
colnames(mat1) = c(" ", seq_1)
rownames(mat1) = c(" ", seq_2)

mat1

