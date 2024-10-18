using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Patients
{
    public interface IPatientRepository : IRepository<Patient, PatientId>
    {
        Task<Patient> GetByEmailAsync(string email);
        Task<Patient> GetByPhoneNumberAsync(string phoneNumber);
        Task<Patient> GetByMedicalRecordNumberAsync(string medicalRecordNumber);
    }
}