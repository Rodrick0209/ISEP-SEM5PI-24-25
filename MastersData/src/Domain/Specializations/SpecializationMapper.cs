using DDDSample1.Domain.Specializations;
using DDDSample1.Application.Dtos;
using System;

namespace DDDSample1.Application.Mappers
{
    public static class SpecializationMapper
    {
        public static SpecializationDto ToDto(this Specialization specialization)
        {
            Console.WriteLine("Entrou no ToDto");
            return new SpecializationDto(specialization.Id.AsString(), specialization.Name);
        }

        public static Specialization ToEntity(this SpecializationDto dto)
        {
            return new Specialization(dto.Name);
        }
    }
}