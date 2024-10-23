using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Patients
{
    public interface IPatientRepository : IRepository<Patient, PatientId>
    {
        Task<Patient> GetByEmailAsync(string email);
        Task<Patient> GetByPhoneNumberAsync(string phoneNumber);
        Task<List<Patient>> GetByNameAsync(string name);
        Task<Patient> GetByMedicalRecordNumberAsync(string medicalRecordNumber);
        Task<List<Patient>> GetByFiltersAsync(string medicalRecordNumber, string name, string email, string dateOfBirth);
        Task<Patient> GetByNameEmailPhoneAsync(string name, string email, string phoneNumber);
        Task<List<Patient>> GetByIdsAsync(List<String> ids);

    }
}