using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Appointments
{
    public interface IAppointmentRepository : IRepository<Appointment, AppointmentId>
    {
     Task<List<Appointment>> GetAppointmentsByDateTimeAsync(DateOnly date, TimeOnly time);

    }
}
