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
            return await this.context.StaffMembers.FirstOrDefaultAsync(p => p.Email.email == email);
        }

         public async Task<Staff> GetByPhoneNumberAsync(string phoneNumber)
        {
            return await this.context.StaffMembers.FirstOrDefaultAsync(p => p.PhoneNumber.phoneNumber == phoneNumber);
        }
        
    }
}
