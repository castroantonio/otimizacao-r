####****----.... Esemplo de otimizacao com estrutura de dados do R. ...----****####

#### Configuracao do ambiente ####
loadlibrary <- function(x) {
  if (!require(x,character.only = TRUE)) {
    install.packages(x, repos='http://cran.us.r-project.org', dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

loadlibrary("profvis")  ## R Profile - permite visualizar pontos de lentidao no codigo fonte



#### **** Usando dataframes **** ####

profvis({  ## gera o profile do codigo
  
  ## variaveis auxiliares
  vetor_logico <- c(rep(FALSE, 10))  ## vetor com 10 posicoes com conteudo FALSE
  minusculas_maiusculas <- c(letters, LETTERS)  ## vetor com 52 posicoes 
  nr_elementos <- length(minusculas_maiusculas)
  
  
  ## estruturas de dados
  aluno <- data.frame("nome", "idade", "respostas", "disciplinas", "historico", stringsAsFactors=FALSE)
  colnames(aluno) <- c("nome", "idade", "respostas", "disciplinas", "historico")
  aluno <- aluno[0, ]			
  
  boletim <- data.frame("disciplina", "nota1"=0, "nota2"=0, "nota3"=0, stringsAsFactors=FALSE)
  colnames(boletim) <- c("disciplina", "nota1", "nota2", "nota3")
  boletim <- boletim[0, ]
  
  
  ## popular estruturas de dados
  novo_elemento <- data.frame("disciplina", "nota1"=0, "nota2"=0, "nota3"=0, stringsAsFactors=FALSE)
  colnames(novo_elemento) <- c("disciplina", "nota1", "nota2", "nota3")
  
  ## criando boletim com 52 disciplinas com respectivas notas
  for(i in 1:nr_elementos) {
    novo_elemento$disciplina <- minusculas_maiusculas[i]
    novo_elemento$nota1 <- i %% 2
    novo_elemento$nota2 <- i %% 8
    novo_elemento$nota3 <- i %% 10
    
    boletim <- rbind(boletim, novo_elemento)
  }
  
  novo_elemento <- data.frame("nome", "idade", "respostas", "disciplinas", "historico", stringsAsFactors=FALSE)
  colnames(novo_elemento) <- c("nome", "idade", "respostas", "disciplinas", "historico")
  
  ## criando 52 alunos alunos
  for (i in 1:nr_elementos) {
    novo_elemento$nome <- minusculas_maiusculas[i]
    novo_elemento$idade <- i
    respostas <- vetor_logico
    respostas[i %% 10 + 1] <- TRUE
    novo_elemento$respostas[1] <- list(respostas)
    
    ## novo_elemento$disciplinas <- boletim  **** nao funciona
    ## novo_elemento$disciplinas[1] <- boletim  **** nao funciona
    ## novo_elemento$disciplinas[[1]] <- boletim  **** nao funciona
    novo_elemento$disciplinas[1] <- list(boletim)
    novo_elemento$historico[1] <- list(boletim)
    
    aluno <- rbing(aluno, novo_elemento)
  }
  
  
  ## de cada aluno obter a nota1 (0) da disciplina de numero 10 (j) , somar com a nota3 (6) do historico 36 (J) e exibir o resultado (6)
  for (i in 1:nr_elementos) {
    resultado <- aluno[i,]$disciplinas[[1]][10, ]$nota1
    resultado <- resultado + aluno[i,]$historico[[1]][36, ]$nota3
    print(resultado)
  }
  
  ## para cada aluno obter as notas do historico e fazer uma media atribuindo a nota1, tambem do historico, e exibindo-a
  for (i in 1:nr_elementos) {  ## loop dos alunos
    for (j in 1:nr_elementos) {  ## loop das disciplinas
      media <- (aluno[i,]$disciplinas[[1]][j, ]$nota1 + aluno[i,]$disciplinas[[1]][j, ]$nota2 + aluno[i,]$disciplinas[[1]][j, ]$nota3) / 3
      aluno[i,]$disciplinas[[1]][j, ]$nota1 <- media
      print(aluno[i,]$disciplinas[[1]][j, ]$nota1)  ## podia ser media, fiz dessa forma para deixar mais lento
    }
  }
  
}) ## profvis



#### **** Usando listas e matrizes **** ####

profvis({  ## gera o profile do codigo
  
  ## variaveis auxiliares
  vetor_logico <- c(rep(FALSE, 10))  ## vetor com 10 posicoes com conteudo FALSE
  minusculas_maiusculas <- c(letters, LETTERS)  ## vetor com 52 posicoes 
  nr_elementos <- length(minusculas_maiusculas)
  
  
  ## estruturas de dados
  aluno <- list()  ## "nome", "idade", "respostas", "disciplinas", "historico"
  
  boletim <- list()  ## "disciplina", "nota1", "nota2", "nota3"
  
  
  ## popular estruturas de dados
  
  ## criando boletim com 52 disciplinas com respectivas notas
  for(i in 1:nr_elementos) {
    disciplina <- minusculas_maiusculas[i]
    nota1 <- i %% 2
    nota2 <- i %% 8
    nota3 <- i %% 10
    novo_elemento <- list(disciplina=disciplina, nota1=nota1, nota2=nota2, nota3=nota3)
    
    boletim[[i]] <- novo_elemento
  }
  
  ## criando 52 alunos alunos
  for (i in 1:nr_elementos) {
    respostas <- vetor_logico
    respostas[i %% 10 + 1] <- TRUE
    
    novo_elemento <- list(nome=minusculas_maiusculas[i], idade=i, respostas=respostas, disciplinas=boletim, historico=boletim)
    
    aluno[[i]] <- novo_elemento
  }
  
  
  ## de cada aluno obter a nota1 (0) da disciplina de numero 10 (j) , somar com a nota3 (6) do historico 36 (J) e exibir o resultado (6)
  for (i in 1:nr_elementos) {
    resultado <- aluno[[i]]$disciplinas[[10]]$nota1
    resultado <- resultado + aluno[[5]]$disciplinas[[36]]$nota3
    print(resultado)
  }
  
  ## para cada aluno obter as notas do historico e fazer uma media atribuindo a nota1, tambem do historico, e exibindo-a
  for (i in 1:nr_elementos) {  ## loop dos alunos
    for (j in 1:nr_elementos) {  ## loop das disciplinas
      media <- (aluno[[i]]$disciplinas[[j]]$nota1 + aluno[[i]]$disciplinas[[j]]$nota2 + aluno[[i]]$disciplinas[[j]]$nota3) / 3
      aluno[[i]]$disciplinas[[j]]$nota1 <- media
      print(aluno[[i]]$disciplinas[[j]]$nota1)  ## podia ser media, fiz dessa forma para deixar mais lento
    }
  }
  
}) ## profvis




