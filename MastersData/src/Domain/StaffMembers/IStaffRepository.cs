using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.StaffMembers
{
    public interface IStaffRepository : IRepository<Staff, StaffId>
    {
        Task DeleteAsync(StaffId id);
        Task<List<Staff>> GetByEmailAsync(string email);
        Task<List<Staff>> GetByPhoneNumberAsync(string phoneNumber);
        Task<List<Staff>> GetByLicenseNumberAsync(string licenseNumber);
        Task<List<Staff>> GetByNameAsync(string name);
        Task<List<Staff>> GetByIdsAsync(string id);

        Task<List<Staff>> GetByFiltersAsync(string name, string licenseNumber, string phoneNumber, string email);

    }
}