using System;
using System.Collections.Generic;
using System.Linq;
using DDDSample1.Application.Mappers;
using DDDSample1.Domain.Specializations;
using DDDSample1.Infrastructure.Specializations;

namespace DDDSample1.Domain.OperationTypes
{
    public class PhaseMapper
    {

        public static PhaseDTO ToPhaseDto(Phase phase)
        {
            
            return new PhaseDTO(
                phase.Id.AsGuid(),
                phase.duration,
                phase.requiredStaff?.Select(ToRequiredStaffDto).ToList() ?? new List<RequiredStaffDTO>()
            );
        }

        public static Phase ToPhaseEntity(PhaseDTO phaseDto,Dictionary<string, Guid> map)
        {
            if (phaseDto == null)
            {
                throw new ArgumentNullException(nameof(phaseDto), "PhaseDto cannot be null");
            }

            return new Phase(
                Guid.NewGuid(),
                phaseDto.Duration, 
                phaseDto.RequiredStaff.Select(dto => ToRequiredStaffEntity(dto, map)).ToList()
                );

        }


        private static RequiredStaff ToRequiredStaffEntity(RequiredStaffDTO dto,Dictionary<string, Guid> map)
        {
            if (dto == null)
            {
                throw new ArgumentNullException(nameof(dto), "RequiredStaffDTO cannot be null");
            }

            return new RequiredStaff(
                int.Parse(dto.num),
                new SpecializationId(map[dto.Specialization])
                );
        }


        private static RequiredStaffDTO ToRequiredStaffDto(RequiredStaff entity)
        {
            if (entity == null)
            {
                throw new ArgumentNullException(nameof(entity), "RequiredStaff cannot be null");
            }

            return new RequiredStaffDTO(entity.num.ToString(), entity.specialization.Value);
        }

    }
}