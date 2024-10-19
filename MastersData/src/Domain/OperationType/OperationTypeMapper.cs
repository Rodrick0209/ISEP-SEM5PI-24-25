using System;
using DDDSample1.Application.Mappers;
using DDDSample1.Domain.Specializations;
using DDDSample1.Infrastructure.Specializations;

namespace DDDSample1.Domain.OperationType
{
    public class OperationTypeMapper
    {
        public static OperationTypeDto ToDto(OperationType operationType)
        {
            return new OperationTypeDto(
                operationType.Id.AsString(), // Assuming Id is of type Guid
                operationType.name,
                operationType.status ? "active" : "inactive", // Mapping boolean status to string
                ToPhaseDto(operationType.preparationPhase),
                ToPhaseDto(operationType.surgeryPhase),
                ToPhaseDto(operationType.cleaningPhase),
                operationType.specialization.Value
            );
            
        }

        public static OperationType toDomain(OperationTypeDto operationTypeDto)
        {
        
            return new OperationType(
                operationTypeDto.Id, // Assuming you have a constructor that accepts an OperationTypeId
                operationTypeDto.Name,
                operationTypeDto.Status == "active", // Mapping string status back to boolean
                ToPhaseEntity(operationTypeDto.PreparationPhase),
                ToPhaseEntity(operationTypeDto.SurgeryPhase),
                ToPhaseEntity(operationTypeDto.CleaningPhase),
                new SpecializationId(operationTypeDto.Specialization)
            );
        }

        private static PhaseDto ToPhaseDto(Phase phase)
        {
            return new PhaseDto(
                phase.AsString()
            );
        }

        public static Phase ToPhaseEntity(PhaseDto phaseDto)
        {
            if (phaseDto == null)
            {
                throw new ArgumentNullException(nameof(phaseDto), "PhaseDto cannot be null");
            }

            return new Phase(phaseDto.Duration);
            
        }

    }
}