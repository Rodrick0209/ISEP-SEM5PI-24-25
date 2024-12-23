using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Domain.Specializations;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;

namespace DDDSample1.Infrastructure.AvailabilitySlots
{

    public class AvailabilitySlotRepository : BaseRepository<AvailabilitySlot, AvailabilitySlotsId>, IAvailabilitySlotsRepository
    {

        private readonly DDDSample1DbContext context;


        public AvailabilitySlotRepository(DDDSample1DbContext context) : base(context.AvailabilitySlots)
        {
            this.context = context;
        }

        public async Task<AvailabilitySlot> GetByIdAsync(SpecializationId id)
        {
            return await this.context.AvailabilitySlots.FirstOrDefaultAsync(p => p.Id == id);
        }

        public async Task<List<AvailabilitySlot>> GetAllAsync()
        {
            return await this.context.AvailabilitySlots
                .Include(slot => slot.Availability) // Inclui a lista de DailyAvailability
                .ToListAsync(); // Retorna uma lista
        }


        public async Task<AvailabilitySlot> GetByStaffIdAsync(string staffId)
        {
            return await this.context.Set<AvailabilitySlot>()
                    .Include(slot => slot.Availability)
                    .FirstOrDefaultAsync(slot => slot.StaffId == staffId)
                    ;
        }




    }

}

