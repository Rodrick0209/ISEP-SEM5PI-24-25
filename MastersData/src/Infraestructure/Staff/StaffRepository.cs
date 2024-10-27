using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Infrastructure.Shared;
using DDDSample1.Domain.StaffMembers;
using Microsoft.AspNetCore.Builder.Extensions;
using Microsoft.EntityFrameworkCore;

namespace DDDSample1.Infrastructure.StaffMembers
{
    public class StaffRepository : BaseRepository<Staff, StaffId>, IStaffRepository
    {
        private readonly DDDSample1DbContext context;

        public StaffRepository(DDDSample1DbContext context) : base(context.StaffMembers)
        {
            this.context = context;
        }

        public async Task<List<Staff>> GetByEmailAsync(string email)
        {
            return await this.context.StaffMembers
            .Where(p => p.Email.email == email)
            .ToListAsync();
        }

         public async Task<List<Staff>> GetByPhoneNumberAsync(string phoneNumber)
        {
            return await this.context.StaffMembers
            .Where(p => p.PhoneNumber.phoneNumber == phoneNumber)
            .ToListAsync();
        }
        public async Task DeleteAsync(StaffId id)
        {
            var staff = await this.context.StaffMembers.FindAsync(id);
            if (staff != null)
            {
            this.context.StaffMembers.Remove(staff);
            await this.context.SaveChangesAsync();
            }
        }

        public async Task<List<Staff>> GetByLicenseNumberAsync(string licenseNumber)
        {
            return await this.context.StaffMembers
            .Where(p => p.LicenseNumber.licenseNumber == licenseNumber)
            .ToListAsync();
        }
        
         public async Task<List<Staff>> GetByNameAsync(string name)
        {
            return await this.context.StaffMembers
                .Where(s => s.FullName.fullName == name)
                .ToListAsync();
        }

        public async Task<List<Staff>> GetByIdsAsync(string id)
        {
            return await this.context.StaffMembers
                .Where(s => id.Contains(s.Id.AsString()))
                .ToListAsync();
        }


        public Task<List<Staff>> GetByFiltersAsync(string name, string licenseNumber, string phoneNumber, string email)
        {
            return this.context.StaffMembers
                .Where(s => s.FullName.fullName.Contains(name)
                            && s.LicenseNumber.licenseNumber.Contains(licenseNumber)
                            && s.PhoneNumber.phoneNumber.Contains(phoneNumber) 
                            && s.Email.email.Contains(email))
                            .ToListAsync();
        }
        
    }
}
