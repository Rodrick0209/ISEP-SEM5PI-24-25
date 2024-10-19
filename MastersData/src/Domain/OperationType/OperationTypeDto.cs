using DDDSample1.Application.Dtos;
using DDDSample1.Domain.Specializations;

namespace DDDSample1.Domain.OperationType
{
    public class OperationTypeDto
    {
        public string Id { get; set; } // Changed to string
        public string Name { get; set; }
        public string Status { get; set; } // Changed to string (could be "active"/"inactive")
        public PhaseDto PreparationPhase { get; set; }
        public PhaseDto SurgeryPhase { get; set; }
        public PhaseDto CleaningPhase { get; set; }

        public string Specialization { get; set; }

        public OperationTypeDto(string operationTypeId, string name, string status, PhaseDto preparationPhase, PhaseDto surgeryPhase, PhaseDto cleaningPhase, string specialization)
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

    public class PhaseDto
    {
    
        public string Duration { get; set; } // Changed to string (duration in minutes as string)

        public PhaseDto(string duration)
        {
            Duration = duration;
        }
    }
}
