using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Appointments;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Appointments
{
    public class AppointmentService : IAppointmentService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IAppointmentRepository _appointmentRepository;

        public AppointmentService(IUnitOfWork unitOfWork, IAppointmentRepository appointmentRepository)
        {
            this._unitOfWork = unitOfWork;
            this._appointmentRepository = appointmentRepository;
        }

        public async Task<List<Appointment>> GetAllAsync()
        {
            return await this._appointmentRepository.GetAllAsync();
        }


    }
}
