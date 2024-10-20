using System;
using DDDSample1.Application.Mappers;
using DDDSample1.Domain.Specializations;
using DDDSample1.Infrastructure.Specializations;

namespace DDDSample1.Domain.OperationType
{
    public class OperationTypeMapper
    {
        public static OperationTypeDto ToDto(OperationType operationType, string specialization)
        {
            return new OperationTypeDto(
                operationType.Id.AsString(), // Assuming Id is of type Guid
                operationType.name,
                operationType.status ? "active" : "inactive", // Mapping boolean status to string
                PhaseMapper.ToPhaseDto(operationType.preparationPhase),
                PhaseMapper.ToPhaseDto(operationType.surgeryPhase),
                PhaseMapper.ToPhaseDto(operationType.cleaningPhase),
                specialization
            );
            
        }

        public static OperationType toDomain(OperationTypeDto operationTypeDto)
        {
        
            return new OperationType(
                operationTypeDto.Id, // Assuming you have a constructor that accepts an OperationTypeId
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