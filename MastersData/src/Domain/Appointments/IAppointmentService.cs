
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
        Task<AppointmentDto> AddAsync(CreatingAppointmentDto appointmentDto);
        Task<AppointmentDtoUI> GetByIdAsync(AppointmentId id);
        Task<Appointment> GetByIdStringAsync(string id);
        Task<EditingAppointmentDto> UpdateAsync(EditingAppointmentDto appointmentDto);
        Task<List<AppointmentDtoUI>> GetAllForUIAsync();
        Task<List<AppointmentDtoInTable>> GetByMedicalRecordNumberAsync(string medicalRecordNumber);
        Task<AppointmentDto> AddWithMedicalTeamAsync(CreateAppointmentWithMedicalTeam appointmentDto);
        Task<StaffForSurgeryDto> GetStaffAvailableForDoinSurgeryAtCertainTime(string startMinute, string date, string appointmentId);
        


    }


}