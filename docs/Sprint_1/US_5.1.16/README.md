# US054 - Como gestor de Logistica pretendo inibir um camião.

# 1. Análise

#### Requisitos funcionais

*Doctors can create an operation request by selecting the patient, operation type, priority, and suggested deadline.
*The system validates that the operation type matches the doctor’s specialization.
*The operation request includes: Patient ID,Doctor ID,Operation Type,Deadline,Priority.
*The system confirms successful submission of the operation request and logs the request in the patient’s medical history.
*The history of the operation type definition is part of the application's data. if the user needs to view the details of an operation that was performed last year, they need to be able to see the operation configuration that was in place at that time.


#### Regras de negócio

*There are three types of Surgery priority : Eletric Surgery, Urgency Surgery, Emergency Surgery (from less priority to most).
*If a surgery overcome the time stipulated it has to be rescheduled.
* The operation type must match the doctor`s specialization.

#### Partes interessadas

*The interested party in this US is the doctor.

#### Pré-condições

*Only doctors can create an operation request.
*Doctor must be logged in.
 
#### Pós-condições

*After the operation request is created it must be persisted in the dataBase.
*The operation request should be available for being evaluated and accordingly to that becoming an appointment.  

## Nível 1 - Vista Processo:
![N1_VP_US16](docs/Sprint_1/US_5.1.16/L1/L1view.svg)
