using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Appointments;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;

namespace DDDSample1.Infrastructure.Appointments
{
    public class AppointmentRepository : BaseRepository<Appointment, AppointmentId>, IAppointmentRepository
    {
        private readonly DDDSample1DbContext context;

        public AppointmentRepository(DDDSample1DbContext context) : base(context.Appointments)
        {
            this.context = context;
        }

        public async Task<List<Appointment>> GetAllAsync()
        {
            return await this.context.Appointments
                .ToListAsync(); // Retorna uma lista
        }
    

        
    }
}
