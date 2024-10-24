# US5.1.17 - As a Doctor, I want to update an operation requisition, so that the Patient has access to the necessary healthcare

# 1. Análise

#### Requisitos funcionais

*Doctors can update operation requests they created (eg., change the deadline or priority).
*The system checks that only the requesting doctor can update the operation request.
*The system logs all updates to the operation request (e.g., changes to priority or deadline).
*Updated requests are reflected immediately in the system and notify the Planning Module of any changes.

#### Regras de negócio

*Deadline can´t be in the past.
*the doctor can change the deadline, the priority, and the description. the doctor cannot change the operation type nor the patient

#### Partes interessadas

*The interested parts in this US are the doctor and the pacient.

#### Pré-condições

*Only doctors that created the operation request can change it.
*Doctor must be logged in.
 
#### Pós-condições

*After the operation request is updated it must be persisted in the dataBase and the change must be registed so we have a historical data.

## Nível 1 - Vista Processo:
![N1_VP_US17](docs/Sprint_1/US_5.1.17/L1/L1view.svg)

# 2. Design

## Nível 2 - Vista Processo:
![N2_VP_US17](docs/Sprint1/US_5.1.17/L2/L2view.svg)

##  Padrões Aplicados

* Padrão GRASP (General Responsibility Assignment Software Patterns), utilizado na criação de controladores para atribuir a responsabilidade de manipular eventos do sistema para uma classe que não seja de interface do usuário (UI);

* Padrão CRUD (acrónimo do inglês Create, Read, Update and Delete) são as quatro operações básicas utilizadas em bases de dados relacionais fornecidas aos utilizadores do sistema, assim como em muitos serviços HTTP.

* Padrão SOLID (acrónimo do inglês Single Responsibility Principle, Open-Closed Principle, Liskov Substitution Principle, Interface Segregation Principle, Dependency Inversion Principle), princípios que se aplicam a qualquer design orientado a objetos, são a filosofia central para metodologias como desenvolvimento software adaptável.

* Padrão DTO (Data Transfer Objects), na criação de estruturas de dados simples que não contêm lógica de negócios.


# Implementação
![N3_VP_US16](docs/Sprint1/US_5.1.17/L3/L3view.svg)

# Observações