using System;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Specializations;

namespace DDDSample1.Domain.OperationTypes
{

    public class OperationType : Entity<OperationTypeId>, IAggregateRoot
    {

        public string name { get; private set; }

        public bool status { get; private set; }

        public Phase preparationPhase { get; private set; }
        public Phase surgeryPhase { get; private set; }
        public Phase cleaningPhase { get; private set; }

        public SpecializationId specialization { get; private set; }


        private OperationType() { }



        public OperationType(string name, bool status, Phase preparationPhase, Phase surgeryPhase, Phase cleaningPhase, SpecializationId specialization)
        {
            if (string.IsNullOrWhiteSpace(name))
            {
                throw new ArgumentNullException(nameof(name), "Name cannot be null or empty.");
            }

            this.Id = new OperationTypeId(Guid.NewGuid());
            this.name = name;
            this.status = status;
            this.preparationPhase = preparationPhase ?? throw new ArgumentNullException(nameof(preparationPhase));
            this.surgeryPhase = surgeryPhase ?? throw new ArgumentNullException(nameof(surgeryPhase));
            this.cleaningPhase = cleaningPhase ?? throw new ArgumentNullException(nameof(cleaningPhase));
            this.specialization = specialization ?? throw new ArgumentNullException(nameof(specialization));
        }



        public void Deactivate()
        {
            this.status = false;
        }


    }







}