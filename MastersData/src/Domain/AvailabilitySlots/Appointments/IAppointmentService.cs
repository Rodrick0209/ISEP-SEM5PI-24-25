
using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Appointments;


namespace DDDSample1.Domain.Appointments
{

    public interface IAppointmentService
    {

        Task<List<Appointment>> GetAllAsync();


    }


}