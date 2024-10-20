using System;
using DDDSample1.Application.Mappers;
using DDDSample1.Domain.Specializations;
using DDDSample1.Infrastructure.Specializations;

namespace DDDSample1.Domain.OperationTypes
{
    public class OperationTypeMapper
    {
        public static OperationTypeDto ToDto(OperationType operationType)
        {
            return new OperationTypeDto(
                operationType.Id.AsGuid(), // Assuming Id is of type Guid
                operationType.name,
                operationType.status ? "active" : "inactive", // Mapping boolean status to string
                PhaseMapper.ToPhaseDto(operationType.preparationPhase),
                PhaseMapper.ToPhaseDto(operationType.surgeryPhase),
                PhaseMapper.ToPhaseDto(operationType.cleaningPhase),
                operationType.specialization.Value
            );
            
        }

        public static OperationType toDomain(OperationTypeDto operationTypeDto)
        {
        
            return new OperationType(
                operationTypeDto.Name,
                operationTypeDto.Status == "active", // Mapping string status back to boolean
                PhaseMapper.ToPhaseEntity(operationTypeDto.PreparationPhase),
                PhaseMapper.ToPhaseEntity(operationTypeDto.SurgeryPhase),
                PhaseMapper.ToPhaseEntity(operationTypeDto.CleaningPhase),
                new SpecializationId(operationTypeDto.Specialization)
            );
        }

    }
}