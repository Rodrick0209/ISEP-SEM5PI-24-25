using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Patients;
using DDDSample1.Infrastructure.Shared;
using Microsoft.AspNetCore.Builder.Extensions;
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

        public Task<List<Patient>> GetByFiltersAsync(string medicalRecordNumber, string name, string email, string dateOfBirth)
        {
            return this.context.Patients
                .Where(p => p.MedicalRecordNumber._medicalRecordNumber.Contains(medicalRecordNumber.Trim()) || p.FullName.fullName.Contains(name.Trim()) || p.Email.email.Contains(email.Trim()) || p.DateOfBirth.dateOfBirth.ToString("yyyy-MM-dd") == dateOfBirth)
                .ToListAsync();
        }

        public async Task<Patient> GetByMedicalRecordNumberAsync(string medicalRecordNumber)
        {
            return await this.context.Patients.FirstOrDefaultAsync(p => p.MedicalRecordNumber._medicalRecordNumber == medicalRecordNumber);
        }

        public async Task<Patient> GetByPhoneNumberAsync(string phoneNumber)
        {
            return await this.context.Patients.FirstOrDefaultAsync(p => p.PhoneNumber.phoneNumber == phoneNumber);
        }
    }
}
