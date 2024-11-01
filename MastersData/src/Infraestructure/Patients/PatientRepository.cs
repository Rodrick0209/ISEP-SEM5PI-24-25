using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Patients;
using DDDSample1.Infrastructure.Shared;
using Microsoft.AspNetCore.Builder.Extensions;
using Microsoft.EntityFrameworkCore;
using Org.BouncyCastle.Asn1;

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

        public async Task<List<Patient>> GetByFiltersAsync(string medicalRecordNumber, string name, string email, string dateOfBirth)
        {
            var query = this.context.Patients.AsQueryable();

            if (!string.IsNullOrWhiteSpace(medicalRecordNumber))
            {
                query = query.Where(p => p.MedicalRecordNumber._medicalRecordNumber.Contains(medicalRecordNumber.ToLower()));
            }

            if (!string.IsNullOrWhiteSpace(name))
            {
                query = query.Where(p => p.FullName.fullName.ToLower().Contains(name.ToLower()));
            }

            if (!string.IsNullOrWhiteSpace(email))
            {
                query = query.Where(p => p.Email.email.ToLower().Contains(email.ToLower()));
            }

            if (!string.IsNullOrWhiteSpace(dateOfBirth))
            {
                query = query.Where(p => p.DateOfBirth.dateOfBirth.ToString("yyyy-MM-dd").Contains(dateOfBirth));
            }

            return await query.ToListAsync();
        }

        public async Task<Patient> GetByMedicalRecordNumberAsync(string medicalRecordNumber)
        {
            return await this.context.Patients.FirstOrDefaultAsync(p => p.MedicalRecordNumber._medicalRecordNumber == medicalRecordNumber);
        }

        public async Task<Patient> GetByNameEmailPhoneAsync(string name, string email, string phoneNumber)
        {
            return await this.context.Patients
                .Where(p => p.FullName.fullName == name
                            && p.Email.email == email
                            && p.PhoneNumber.phoneNumber == phoneNumber)
                .FirstOrDefaultAsync();
        }

        public async Task<Patient> GetByPhoneNumberAsync(string phoneNumber)
        {
            return await this.context.Patients.FirstOrDefaultAsync(p => p.PhoneNumber.phoneNumber == phoneNumber);
        }


        public async Task<List<Patient>> GetByIdsAsync(List<String> ids)
        {
            return await this.context.Patients
                .Where(p => ids.Contains(p.Id.AsString()))
                .ToListAsync();
        }

        public async Task<List<Patient>> GetByNameAsync(string name)
        {
            return await this.context.Patients
                .Where(p => p.FullName.fullName == name)
                .ToListAsync();
        }




        public async Task<Patient> GetLastPatientRegisteredInMonthAsync()
        {
            return await this.context.Patients
                .Where(p => p.MedicalRecordNumber._medicalRecordNumber.Substring(0, 6) == DateTime.Now.ToString("yyyyMM"))
                .OrderByDescending(p => p.MedicalRecordNumber._medicalRecordNumber)
                .FirstOrDefaultAsync();
        }
    }
}
