# Sistema de Gerenciamento do Complexo Esportivo (SGCE) - UFCG :school:

## O QUE É?

O Sistema de Gerenciamento de Complexo Esportivo (SGCE) para a Universidade Federal de Campina Grande (UFCG) surge como uma solução inovadora para otimizar e aprimorar a gestão de locais destinados à realização de eventos diversos dentro do complexo esportivo. Com uma comunidade acadêmica dinâmica e uma vasta programação de atividades, é essencial contar com uma plataforma eficiente que facilite o agendamento, controle e organização de espaços das quadras esportivas.

- [Link para a especificação](https://docs.google.com/document/d/1-Hnz4GQ-bMf2XOeH2zSjqXCXKktow_ctU4qSig7bf1I/edit#bookmark=id.gjdgxs)

## COMO EXECUTAR - HASKELL

### Setup inicial

- `Gum`: Ferramenta para scripts de shell.

```bash
# Debian/Ubuntu
sudo mkdir -p /etc/apt/keyrings
curl -fsSL https://repo.charm.sh/apt/gpg.key | sudo gpg --dearmor -o /etc/apt/keyrings/charm.gpg
echo "deb [signed-by=/etc/apt/keyrings/charm.gpg] https://repo.charm.sh/apt/ * *" | sudo tee /etc/apt/sources.list.d/charm.list
sudo apt update && sudo apt install gum

# Windows (via WinGet or Scoop)
winget install charmbracelet.gum
scoop install charm-gum
```

- `Gnuplot`: Ferramenta para plotar gráficos.

```bash
sudo apt-get update
sudo apt-get install gnuplot
```

### Rodando o sistema

```bash
cabal run
```

## DEMONSTRAÇÃO
- [Link para o vídeo](https://youtu.be/2yTaIxSlL5E)

## GRUPO
| [<img src="https://avatars.githubusercontent.com/u/83247917?v=4" width="120px;" /><br /><sub><b>Samuel Cabral</b></sub>](https://github.com/samuelcluna)<br /> | [<img src="https://avatars.githubusercontent.com/u/138733512?v=4 " width="120px;"/><br /><sub><b>Marcos Antônio</b></sub>](https://github.com/W00kyz)<br /> | [<img src="https://avatars.githubusercontent.com/u/83998722?v=4" width="120px;"/><br /><sub><b>Luana Bringel</b></sub>](https://github.com/luanabringel)<br /> | [<img src="https://avatars.githubusercontent.com/u/101367401?v=4" width="120px;"/><br /><sub><b>Anna Beatriz</b></sub>](https://github.com/Beatriz-Furtado)<br> | [<img src="https://avatars.githubusercontent.com/u/92611014?v=4" width="120px;"/><br /><sub><b>Paola Moura</b></sub>](https://github.com/paolamoura)<br /> |
| :---: | :---: | :---: | :---: | :---: |
