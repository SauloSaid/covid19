# Extrair dados sobre COVID da Universidade de Johns Hopkins
## Transformando dados e calculando campos importantes para análise
A Universidade de Johns Hopkins mantem um dos mais famosos levantamentos sobre a epidemia causado pelo Corona Vírus. Eles disponibilizam esses dados em um reposit[orio do Github [Github](https://github.com/CSSEGISandData/COVID-19).

Entretanto, o formato dos dados náo é dos mais adequados. Cada data é uma nova coluna. Além disso, dados importantes náo estáo pré-calculados nessa base (o que é compreensível: é um repositório). 

O arquivo CSV que criei (e o código que a gerou) possuem algumas vantagens em relação ao disponibilizado pela U. J. Hopkins:
- Base de dados em formato wide
- Traz o número de *novos casos* e *novas mortes*
- Traz o total de dias desde o primeiro caso para cada data
- Traz o total de dias desde a primeira morte para cada data

Os dados fornecidos pela refereida universidade tem como dado mais granular o estado/província. Entretanto, para muitos países náo há quebra por província (como é o caso do *Brasi*).
Favor referenciar o meu [linkedin]() quando utilizar os dados . 


Dados sobre o coronavirus
