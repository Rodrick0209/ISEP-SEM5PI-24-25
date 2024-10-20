using System;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Specializations;

namespace DDDSample1.Domain.OperationType
{

    public class OperationType : Entity<OperationTypeId>,IAggregateRoot
    {
        //Quem fizer esta Us adicione os atributos criei so para poder user o id na minha US
        //public OperationTypeId operationTypeId { get; private set; }
        
        public string name { get; private set; }

        public bool status { get; private set; }

        public Phase preparationPhase { get; private set; }
        public Phase surgeryPhase { get; private set; }
        public Phase cleaningPhase { get; private set; }

        public SpecializationId specialization {get; private set;}

        private OperationType() { }


        public OperationType(string id,string name, bool status, Phase preparationPhase, Phase surgeryPhase, Phase cleaningPhase, SpecializationId specialization)
        {
            this.Id = new OperationTypeId(id);
            this.name = name;
            this.status = status;
            this.preparationPhase = preparationPhase;
            this.surgeryPhase = surgeryPhase;
            this.cleaningPhase = cleaningPhase;
            this.specialization= specialization;
        }


        public void Deactivate()
        {
            this.status = false;
        }


    }







}