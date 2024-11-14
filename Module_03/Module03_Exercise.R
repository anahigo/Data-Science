# Exercise 1 - Search for the function that allows you to list all files in the working directory
list.files()

# Exercise 2 - Create a dataframe from 3 vectors: one of characters, one of logic and one of numbers
answer2 <- data.frame(Nomes = c("Ana", "Carol", "Carlos"),
                 Logico = c(FALSE, TRUE, FALSE),
                 Idade = c(39, 43, 45))

answer2

# Exercise 3 - Consider the vector below.
# Create a loop that checks for numbers greater than 10 and prints the number and a message to the console.
# **Creating a Vector**
vec1 <- c(12, 3, 4, 19, 34)
vec1

for (i in 1:length(vec1)) {
  if (vec1[i] > 10) {
    print(vec1[i])
    print("Este número é maior que 10")
  } else if (vec1[i] < 10) {
    print(vec1[i])
    print("Este é o número é menor que 10")
  } else {
    print(vec1[i])
    print("Este é o número 10")
  }
}

# Exercise 4 - Consider the list below. Create a loop that prints each element of the list to the console
lst2 <- list(2, 3, 5, 7, 11, 13)
lst2

for (i in 1:length(lst2)) {
  print(lst2[[i]])
}

# Exercise 5 - Consider the two matrices below.
# Do element-wise multiplication and normal multiplication between the matrices
mat1 <- matrix(c(1:50), nrow = 5, ncol = 5, byrow = T)
mat1
mat2 <- t(mat1)
mat2

# Element-wise multiplication
answer5a <- mat1 * mat2
answer5a

# Matrix multiplication
answer5b <- mat1 %*% mat2
answer5b

# Exercise 6 - Create a vector, matrix, list and dataframe and name each of the objects
vetor <- c("Ana", "Carol", "Carlos", "Isabel", "Edson")
names(vetor) <- c("FilhaMaisNova", "FilhaDoMeio", "FilhoMaisVelho", "Mae", "Pai")
vetor

matriz <- matrix(c(1:25), nrow = 5, ncol = 5, byrow = T)
dimnames(matriz) <- (list(c("Linha1", "Linha2", "Linha3", "Linha4", "Linha5"), c("Coluna1", "Coluna2", "Coluna3", "Coluna4", "Coluna5")))
matriz

lista <- list(39, 43, 45, 69, 73)
names(lista) <- c("idadeAna", "idadeCarol", "idadeCarlos", "idadeIsabel", "idadeEdson")
lista

dataframe <- data.frame(c("Ana", "Carol", "Carlos"),
                        c(39, 43, 45),
                        c(FALSE, TRUE, FALSE))
colnames(dataframe) <- c("Nome", "Idade", "Logico")
rownames(dataframe) <- c("FilhaMaisNova", "FilhaDoMeio", "FilhoMaisVelho")
dataframe

# Exercise 7 - Consider the matrix below. Randomly assign NA values to 50 array elements
# Hint: use the sample() function
mat2 <- matrix(1:90, 10)
mat2

?sample

mat2[sample(1:50, 10)] = NA
mat2

# Exercise 8 - For the matrix below, calculate the sum per row and per column
mat1 <- matrix(c(1:50), nrow = 5, ncol = 5, byrow = T)
mat1

rowSums(mat1)

colSums(mat1)

# Exercise 9 - For the vector below, sort the values in ascending order
a <- c(100, 10, 10000, 1000)
a

a[order(a)]

# Exercise 10 - Print in the console all elements of the matrix below that are greater than 15
mat1 <- matrix(c(1:50), nrow = 5, ncol = 5, byrow = T)
mat1

for (i in mat1) {
  if (i > 15) {
    print(i)
  }
}