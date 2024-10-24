# US5.1.16 - As a Doctor, I want to request an operation, so that the Patient has access to the necessary healthcare.

# 1. Análise

#### Requisitos funcionais

*Doctors can create an operation request by selecting the patient, operation type, priority, and suggested deadline.
*The system validates that the operation type matches the doctor’s specialization.
*The operation request includes: Patient ID,Doctor ID,Operation Type,Deadline,Priority.
*The system confirms successful submission of the operation request and logs the request in the patient’s medical history.



#### Regras de negócio

*There are three types of Surgery priority : Eletric Surgery, Urgency Surgery, Emergency Surgery (from less priority to most).
*If a surgery overcome the time stipulated it has to be rescheduled.
*The operation type must match the doctor`s specialization.
*The doctor that creates the operation request doesn´t  necessarily need to be the doctor who will eventually perform the cirurgy.

#### Partes interessadas

*The interested parts in this US is the doctor and the pacient.

#### Pré-condições

*Only doctors can create an operation request.
*Doctor must be logged in.
 
#### Pós-condições

*After the operation request is created it must be persisted in the dataBase.
*The operation request should be available for being evaluated and accordingly to that becoming an appointment.  

## Nível 1 - Vista Processo:
![N1_VP_US16](docs/Sprint_1/US_5.1.16/L1/L1view.svg)


# 2. Design

## Nível 2 - Vista Processo:
![N2_VP_US16](docs/Sprint1/US_5.1.16/L2/L2view.svg)

##  Padrões Aplicados

* Padrão GRASP (General Responsibility Assignment Software Patterns), utilizado na criação de controladores para atribuir a responsabilidade de manipular eventos do sistema para uma classe que não seja de interface do usuário (UI);

* Padrão CRUD (acrónimo do inglês Create, Read, Update and Delete) são as quatro operações básicas utilizadas em bases de dados relacionais fornecidas aos utilizadores do sistema, assim como em muitos serviços HTTP.

* Padrão SOLID (acrónimo do inglês Single Responsibility Principle, Open-Closed Principle, Liskov Substitution Principle, Interface Segregation Principle, Dependency Inversion Principle), princípios que se aplicam a qualquer design orientado a objetos, são a filosofia central para metodologias como desenvolvimento software adaptável.

* Padrão DTO (Data Transfer Objects), na criação de estruturas de dados simples que não contêm lógica de negócios.


# Implementação
![N3_VP_US16](docs/Sprint1/US_5.1.16/L3/L3view.svg)

# Observações