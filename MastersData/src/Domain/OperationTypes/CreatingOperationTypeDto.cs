using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.OperationTypes
{
    public class CreatingOperationTypeDto
    {
        public OperationTypeId OperationTypeId { get; set; }
        public string Name { get; set; }
        public bool Status { get; set; }
        public Phase PreparationPhase { get; set; }
        public Phase SurgeryPhase { get; set; }
        public Phase CleaningPhase { get; set; }

        // Constructor to initialize the DTO
        public CreatingOperationTypeDto(OperationTypeId operationTypeId, string name, bool status, Phase preparationPhase, Phase surgeryPhase, Phase cleaningPhase)
        {
            OperationTypeId = operationTypeId;
            Name = name;
            Status = status;
            PreparationPhase = preparationPhase;
            SurgeryPhase = surgeryPhase;
            CleaningPhase = cleaningPhase;
        }
    }
}
