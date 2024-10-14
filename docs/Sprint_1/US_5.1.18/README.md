# US5.1.18 As a Doctor, I want to remove an operation requisition, so that the healthcare activities are provided as necessary.


# 1. Análise

#### Requisitos funcionais

*Doctors can delete operation requests they created if the operation has not yet been scheduled.
*A confirmation prompt is displayed before deletion.
*Once deleted, the operation request is removed from the patient’s medical record and cannot
be recovered.
*The system notifies the Planning Module and updates any schedules that were relying on this
request.

#### Regras de negócio

*Operation Requests only can be deleted if the operation has not been scheduled
*The operation request is totally removed from the patient`s medical record and connot be recovered in any future time(permanent delete) 

#### Partes interessadas

*The interested parts in this US are the doctor and the pacient, also the planning module.

#### Pré-condições

*Only doctors that created the operation request can change it.
*Doctor must be logged in.
*Operation request must not be scheduled.
 
#### Pós-condições

*Operation Request is permanently removed from the system

## Nível 1 - Vista Processo:
![N1_VP_US18](docs/Sprint_1/US_5.1.18/L1/L1view.svg)

# 2. Design

## Nível 2 - Vista Processo:
![N2_VP_US18](docs/Sprint1/US_5.1.18/L2/L2view.svg)

##  Padrões Aplicados

* Padrão GRASP (General Responsibility Assignment Software Patterns), utilizado na criação de controladores para atribuir a responsabilidade de manipular eventos do sistema para uma classe que não seja de interface do usuário (UI);

* Padrão CRUD (acrónimo do inglês Create, Read, Update and Delete) são as quatro operações básicas utilizadas em bases de dados relacionais fornecidas aos utilizadores do sistema, assim como em muitos serviços HTTP.

* Padrão SOLID (acrónimo do inglês Single Responsibility Principle, Open-Closed Principle, Liskov Substitution Principle, Interface Segregation Principle, Dependency Inversion Principle), princípios que se aplicam a qualquer design orientado a objetos, são a filosofia central para metodologias como desenvolvimento software adaptável.

* Padrão DTO (Data Transfer Objects), na criação de estruturas de dados simples que não contêm lógica de negócios.


# Implementação
![N3_VP_US16](docs/Sprint1/US_5.1.18/L3/L3view.svg)

# Observações