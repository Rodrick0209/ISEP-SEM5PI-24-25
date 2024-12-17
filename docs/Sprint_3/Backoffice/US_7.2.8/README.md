## Implementacoes que faltam backend (Create)

- Q: When a doctor is selecting the staff for an appointment, what should happen if, for every slot he could choose, there aren't enough staffs to perform the operation?
 -A: the appointment cannot be scheduled for that date. the doctor must choose a different date.

 - Q: When the doctor selects the team for the Appointment, that includes doctors and nurses. Regarding cleaners, which staff (doctors, nurses, interns) can be selected for that role? And is there any criteria for that selection?
 -A : Falta verificar que fazem parte d amedical team as opcoes que vao aparecer, ou seja que o Id comeca ou com D ou com N.


 -Q: Regarding the team selected by the doctor when creating the appointment, does this team include only doctors, doctors and anesthetists, or doctors, anesthetists and cleaners?
 -A: Se for feito a perfeiao as possibilidades de medicos vao estar limitadas pelas especificacoes da operation Type.



## Implementacoes que faltam backend (update)

-The doctor must be able to "transform" an existing operation request into an actual appointment by specifying the room, date and team of the surgery. the system must ensure all the resources and personnel is available at the selected time according to the operation type duration.

-After the appointment is planned, it is possible to update the team, room and date. the system must ensure all the resources and personnel is available at the selected time according to the operation type duration.

-Em principio o update esta feito, mas nao tenho nada according operation Type duration, ou seja, so meto staff la para dentro sem limitacoes.


## Implementacoes front end que estao a falhar

- Nao sei o que aconteceu mas agora os get ja estao todos a ir buscar as merdas, ou seja, o maior problema sera decidir o que colocar na operation Request em vez do ID porque neste momento estoua  escolher ids porque nao sabia o que devia meter.

-A parte de escolher as equipas tambem nao sei se esta muito bem.

-Depois so testando.


