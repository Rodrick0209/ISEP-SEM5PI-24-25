using System.Threading.Tasks;
using DDDSample1.Domain.OperationTypes;
using System.Collections.Generic;
using DDDSample1.Domain.Specializations;
using System;
using DDDSample1.Application.Dtos;

namespace DDDSample1.Domain.OperationTypes
{
    public interface ISpecializationService
    {
        Task<Specialization> GetByIdAsync(SpecializationId id);

        Task<Specialization> GetByNameAsync(string name);

        Task<Dictionary<Guid, string>> GetByNameOperationTypeAsync(OperationType op);

        Task<Dictionary<string, Guid>> GetSpecializationMapAsync();

        Task<SpecializationDto> CreateAsync(SpecializationDto dto);

        Task<List<SpecializationDto>> GetAllAsync();

        Task<List<SpecializationDto>> GetFilteredAsync(SpecializationFilterDto dto);

        Task<SpecializationDto> UpdateAsync(SpecializationDto dto);

        Task <SpecializationDto> RemoveAsync(Guid id);


    }
}