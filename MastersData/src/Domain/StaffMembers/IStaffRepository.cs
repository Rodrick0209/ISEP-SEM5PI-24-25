using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.StaffMembers
{
    public interface IStaffRepository : IRepository<Staff, StaffId>
    {
        Task<Staff> GetByEmailAsync(string email);
        Task<Staff> GetByPhoneNumberAsync(string phoneNumber);

    }
}