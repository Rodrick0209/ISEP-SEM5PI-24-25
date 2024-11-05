using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Domain.Shared;
using DDDSample1.Infrastructure.Shared;



namespace DDDSample1.Infrastructure.AvailabilitySlots
{

    public class DailyAvailabilityRepository : BaseRepository<DailyAvailability, DailyAvailabilityId>, IDailyAvailabilityRepository
    {
        private readonly DDDSample1DbContext context;


        public DailyAvailabilityRepository(DDDSample1DbContext context):base(context.DailyAvailabilities)
        {
            this.context = context;
        }


    






    }



}