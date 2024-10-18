using DDDSample1.Domain.Specializations;
using DDDSample1.Application.Dtos;

namespace DDDSample1.Application.Mappers
{
    public static class SpecializationMapper
    {
        public static SpecializationDto ToDto(this Specialization specialization)
        {
            return new SpecializationDto(specialization.Id.Value, specialization.Name);
        }

        public static Specialization ToEntity(this SpecializationDto dto)
        {
            return new Specialization(dto.Name);
        }
    }
}