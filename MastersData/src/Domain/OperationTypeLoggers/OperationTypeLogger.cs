using System;
using DDDSample1.Domain.OperationTypeLoggers;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Specializations;

namespace DDDSample1.Domain.OperationTypes
{

    public class OperationTypeLogger : Entity<OperationTypeLoggerId>,IAggregateRoot
    {
        public OperationTypeId OperationRequestId { get; private set; }

        public string name { get; private set; }

        public bool status { get; private set; }

        public Phase preparationPhase { get; private set; }
        public Phase surgeryPhase { get; private set; }
        public Phase cleaningPhase { get; private set; }

        public SpecializationId specialization { get; private set; }

        public Guid version {get; private set;}


        private OperationTypeLogger() { }



        public OperationTypeLogger(OperationType op)
        {
            this.Id= new OperationTypeLoggerId(Guid.NewGuid());
            this.OperationRequestId = op.Id;
            this.name = op.name;
            this.status = op.status;
            this.preparationPhase = op.preparationPhase;
            this.surgeryPhase = op.surgeryPhase;
            this.cleaningPhase = op.cleaningPhase;
            this.specialization= op.specialization;
            this.version=Guid.NewGuid();
        }


    }







}