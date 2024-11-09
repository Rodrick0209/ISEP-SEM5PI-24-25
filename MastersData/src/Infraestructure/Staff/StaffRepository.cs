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

        public async Task<Staff> GetByEmailAsync(string email)
        {
            return await this.context.StaffMembers.FirstOrDefaultAsync(e => e.Email.email == email);
            
        }

          public async Task<Staff> GetByPhoneNumberAsync(string phoneNumber)
        {
            return await this.context.StaffMembers.FirstOrDefaultAsync(p => p.PhoneNumber.phoneNumber == phoneNumber);
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

        public async Task<Staff> GetByLicenseNumberAsync(string licenseNumber)
        {
           return await this.context.StaffMembers.FirstOrDefaultAsync(ln => ln.LicenseNumber.licenseNumber == licenseNumber);
        }
        
         public async Task<Staff> GetByNameAsync(string name)
        {
            return await this.context.StaffMembers.FirstOrDefaultAsync(n => n.FullName.fullName == name);
        }

        public async Task<Staff> GetByIdsAsync(string id)
        {
            return await this.context.StaffMembers.FirstOrDefaultAsync(s => s.Id.AsString() == id);
                

;
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
