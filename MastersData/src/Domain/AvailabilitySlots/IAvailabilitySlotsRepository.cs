
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.AvailabilitySlots;


namespace DDDSample1.Domain.AvailabilitySlots
{
  public interface IAvailabilitySlotsRepository : IRepository<AvailabilitySlot, AvailabilitySlotsId>
  {
    Task<AvailabilitySlot> GetByIdAsync(AvailabilitySlotsId id);


  }

}