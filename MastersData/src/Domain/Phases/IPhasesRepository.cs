using System.Threading.Tasks;
using DDDSample1.Domain.Shared;




namespace DDDSample1.Domain.OperationTypes
{
    public interface IPhasesRepository:IRepository<Phase, PhasesId>
    {
        Task<Phase> GetByIdAsync(PhasesId id);

    }
}