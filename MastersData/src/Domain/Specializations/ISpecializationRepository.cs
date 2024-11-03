
using System.Threading.Tasks;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Specializations;


namespace DDDSample1.Domain.Specializations
{
  public interface ISpecializationRepository : IRepository<Specialization, SpecializationId>
  {
    Task<Specialization> GetByIdAsync(SpecializationId id);

    Task<Specialization> GetByNameAsync(string name);

      Task<Dictionary<string, SpecializationId>> GetSpecializationMapAsync();


  }

}