using DDDSample1.Application.Dtos;
using DDDSample1.Domain.Specializations;

namespace DDDSample1.Domain.OperationType
{
    public class OperationTypeDto
    {
        public string Id { get; set; } // Changed to string
        public string Name { get; set; }
        public string Status { get; set; } // Changed to string (could be "active"/"inactive")
        public PhaseDTO PreparationPhase { get; set; }
        public PhaseDTO SurgeryPhase { get; set; }
        public PhaseDTO CleaningPhase { get; set; }

        public string Specialization { get; set; }

        public OperationTypeDto(string operationTypeId, string name, string status, PhaseDTO preparationPhase, PhaseDTO surgeryPhase, PhaseDTO cleaningPhase, string specialization)
        {
            Id = operationTypeId;
            Name = name;
            Status = status;
            PreparationPhase = preparationPhase;
            SurgeryPhase = surgeryPhase;
            CleaningPhase = cleaningPhase;
            Specialization = specialization;
        }
    }

}
