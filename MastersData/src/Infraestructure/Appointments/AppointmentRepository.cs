using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Appointments;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;
using Org.BouncyCastle.Asn1.Cms;

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
    

        public async Task<List<Appointment>> GetAppointmentsByDateTimeAsync(DateOnly date, TimeOnly time)
        {
            return await this.context.Appointments
                .Where(a => a.AppointmentTimeSlot.Date == date && 
                            a.AppointmentTimeSlot.TimeSlot.StartMinute <= time.ToTimeSpan().TotalMinutes &&
                            a.AppointmentTimeSlot.TimeSlot.EndMinute >= time.ToTimeSpan().TotalMinutes)
                .ToListAsync();
        }

        
    }
}
