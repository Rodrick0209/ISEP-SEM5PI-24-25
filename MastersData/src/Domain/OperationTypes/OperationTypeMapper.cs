using System;
using System.Collections.Generic;
using System.Linq;
using DDDSample1.Application.Mappers;
using DDDSample1.Domain.Specializations;
using DDDSample1.Infrastructure.Specializations;

namespace DDDSample1.Domain.OperationTypes
{
    public class OperationTypeMapper
    {
        public static OperationTypeDto ToDto(OperationType operationType, string specializationName, Dictionary<Guid, string> specializationNames)
        {
            return new OperationTypeDto(
                operationType.Id.AsGuid(), // Assuming Id is of type Guid
                operationType.name,
                operationType.status ? "active" : "inactive", // Mapping boolean status to string
                TransformPhase(operationType.preparationPhase, specializationNames),
                TransformPhase(operationType.surgeryPhase, specializationNames),
                TransformPhase(operationType.cleaningPhase, specializationNames),
                specializationName
            );

        }

        public static OperationTypeDto ToDto(OperationType operationType)
        {
            return new OperationTypeDto(
                operationType.Id.AsGuid(), // Assuming Id is of type Guid
                operationType.name,
                operationType.status ? "active" : "inactive", // Mapping boolean status to string
                TransformPhase(operationType.preparationPhase, new Dictionary<Guid, string>()),
                TransformPhase(operationType.surgeryPhase, new Dictionary<Guid, string>()),
                TransformPhase(operationType.cleaningPhase, new Dictionary<Guid, string>()),
                operationType.specialization.Value
            );
        }

        private static PhaseDTO TransformPhase(Phase phase, Dictionary<Guid, string> specializationNames)
        {
            return new PhaseDTO
            {
                Id = phase.Id.AsGuid(),
                Duration = phase.duration,
                RequiredStaff = phase.requiredStaff.Select(staff => new RequiredStaffDTO(
                    staff.num.ToString(),
                    specializationNames.ContainsKey(staff.specialization.AsGuid()) ? specializationNames[staff.specialization.AsGuid()] : staff.specialization.AsGuid().ToString()
                )).ToList()
            };
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