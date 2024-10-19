
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Specializations;


namespace DDDSample1.Domain.Specializations
{
public interface ISpecializationRepository : IRepository<Specialization,SpecializationId>
{

}

}