using System;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Patient;
using DDDSample1.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;

namespace DDDSample1.Infrastructure.Patients
{
    public class PatientRepository : BaseRepository<Patient, PatientId>, IPatientRepository
    {
        private readonly DDDSample1DbContext context;

        public PatientRepository(DDDSample1DbContext context) : base(context.Patients)
        {
            this.context = context;
        }
        
        public async Task<Patient> GetByEmailAsync(string email)
        {
            return await this.context.Patients.FirstOrDefaultAsync(p => p.Email.email == email);
        }

        public Task<Patient> GetByPhoneNumberAsync(string phoneNumber)
        {
            return this.context.Patients.FirstOrDefaultAsync(p => p.PhoneNumber.phoneNumber == phoneNumber);
        }

        public async Task<Patient> GetLastPatientInMonthAsync(DateTime now)
        {
            return await this.context.Patients
                .Where(p => p.MedicalRecordNumber._medicalRecordNumber.Contains(now.ToString("yyyyMM")))
                .OrderByDescending(p => int.Parse(p.MedicalRecordNumber._medicalRecordNumber))
                .FirstOrDefaultAsync();
        }
    }
}
