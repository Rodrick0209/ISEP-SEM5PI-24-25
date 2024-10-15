# US5.1.7 - 5.1.7 As a Patient, I want to log in to the healthcare system using my interal IAM credentials.

# 1. Análise

#### Requisitos funcionais

*Patients log in via an internal Identity and Access Management (IAM) provider.
*After successful authentication via the IAM, patients are redirected to the healthcare system with a valid session.
*Patients have access to their appointment history, medical records, and other features relevant to their profile.
*Sessions expire after a defined period of inactivity, requiring reauthentication.
 

#### Partes interessadas

*The interested parts in this US is pacient.

#### Pré-condições

*Patient must be registed in the system so he can log in
 
#### Pós-condições

*Afther the patient is logged in he has access to the patient funcionlities provided by the system 

## Nível 1 - Vista Processo:
![N1_VP_US16](docs/Sprint_1/US_5.1.7/L1/L1view.svg)


# 2. Design

## Nível 2 - Vista Processo:
![N2_VP_US16](docs/Sprint1/US_5.1.7/L2/L2view.svg)

##  Padrões Aplicados

* Padrão GRASP (General Responsibility Assignment Software Patterns), utilizado na criação de controladores para atribuir a responsabilidade de manipular eventos do sistema para uma classe que não seja de interface do usuário (UI);

* Padrão CRUD (acrónimo do inglês Create, Read, Update and Delete) são as quatro operações básicas utilizadas em bases de dados relacionais fornecidas aos utilizadores do sistema, assim como em muitos serviços HTTP.

* Padrão SOLID (acrónimo do inglês Single Responsibility Principle, Open-Closed Principle, Liskov Substitution Principle, Interface Segregation Principle, Dependency Inversion Principle), princípios que se aplicam a qualquer design orientado a objetos, são a filosofia central para metodologias como desenvolvimento software adaptável.

* Padrão DTO (Data Transfer Objects), na criação de estruturas de dados simples que não contêm lógica de negócios.


# Implementação
![N3_VP_US16](docs/Sprint1/US_5.1.7/L3/L3view.svg)

# Observações