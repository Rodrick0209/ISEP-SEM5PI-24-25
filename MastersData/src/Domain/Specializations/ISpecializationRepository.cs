
using System.Threading.Tasks;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Specializations;
using DDDSample1.Application.Dtos;


namespace DDDSample1.Domain.Specializations
{
  public interface ISpecializationRepository : IRepository<Specialization, SpecializationId>
  {

    Task<Specialization> GetByNameAsync(string name);

    Task<Dictionary<string, SpecializationId>> GetSpecializationMapAsync();


    Task<List<Specialization>> GetFilteredAsync(SpecializationFilterDto dto);



  }

}