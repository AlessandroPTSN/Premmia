###########################
### pacotes necessarios ###
###########################

library(rvest)
library(dplyr)
library(ggplot2)
library(stringr)
library(ggfortify)
library(ggdendro)

##########################
### obtencao dos dados ###
##########################


produtos = ""
pontos = 0
sites = ""
a = data.frame(produtos,pontos,sites)
for(i in 0:6){ #1 até 7 são as paginas que tem no premmia até o momento em que o código foi feito
url <- "https://www.petrobraspremmia.com.br/Trocar-Pontos/c/1?q=%3AtopExchanged&page=0"
url = gsub('.{1}$',i, url)


produtos = read_html(url) %>%
  html_nodes("p.name-product-custom") %>%
  html_text()%>%str_replace("\n\t\t\t\t\t", "")

pontos=read_html(url) %>%
  html_nodes("div.price") %>%
  html_text()%>%str_replace("\n\t\t", "")%>%str_replace("\t\t\n\t\t\t\t\n\t\t\t", "")

h=read_html(url) %>%
  html_nodes(".thumb")

sites=c(1:length(h))
for(j in 1:length(h)){
  sites[j]=str_sub(h[j], 25)
  sites[j]=sub(">.*","",sites[j])
  sites[j]=gsub('.{1}$', '', sites[j])
  sites[j]=paste0("https://www.petrobraspremmia.com.br/", sites[j])
}


min = c(length(produtos),length(pontos),length(sites))
if(length(produtos)>min){
  produtos= produtos[1:min]
}
if(length(pontos)>min){
  pontos= pontos[1:min]
}
if(length(sites)>min){
  sites= sites[1:min]
}

pontos=gsub('.{8}$',"", pontos)
pontos=gsub('\\.', '',pontos)
pontos = as.numeric(pontos)
b=data.frame(produtos,pontos,sites)


a = a %>% 
  bind_rows(b)

}

a[is.na(a)] <- 0
a=a[-1,]

c = a[a$pontos<1660,] #Quantidade de pontos que o usuário tem "ofertas até 1660 pontos"

c=c %>% arrange(pontos) #Ordena pela quantidade de pontos (dos menores aos maiores)

#mude o diretório a seu favor
write.csv2(c, file = "C:/Users/Neo/Desktop/premia.csv")#Salva os dados em uma planília


