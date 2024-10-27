using System.Threading.Tasks;
using DDDSample1.Domain.OperationTypes;
using System.Collections.Generic;
using DDDSample1.Domain.Specializations;
using System;

namespace DDDSample1.Domain.OperationTypes
{
    public interface ISpecializationService
    {
        Task<Specialization> GetByIdAsync(SpecializationId id);

        Task<Specialization> GetByNameAsync(string name);

        Task<Dictionary<Guid, string>> GetByNameOperationTypeAsync(OperationType op);

    }
}