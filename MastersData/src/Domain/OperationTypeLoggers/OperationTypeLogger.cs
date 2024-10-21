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

        public string status { get; private set; }

        public string preparationPhase { get; private set; }
        public string surgeryPhase { get; private set; }
        public string cleaningPhase { get; private set; }

        public string specialization {get; private set;}


        private OperationTypeLogger() { }



        public OperationTypeLogger(OperationTypeId id,string name, string status, string preparationPhase, string surgeryPhase, string cleaningPhase, string specialization)
        {
            this.Id= new OperationTypeLoggerId(Guid.NewGuid());
            this.OperationRequestId = id;
            this.name = name;
            this.status = status;
            this.preparationPhase = preparationPhase;
            this.surgeryPhase = surgeryPhase;
            this.cleaningPhase = cleaningPhase;
            this.specialization= specialization;
        }


    }







}