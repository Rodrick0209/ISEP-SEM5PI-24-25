using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.StaffMembers
{
    public interface IStaffRepository : IRepository<Staff, StaffId>
    {
        Task DeleteAsync(StaffId id);
        Task<Staff> GetByEmailAsync(string email);
        Task<Staff> GetByPhoneNumberAsync(string phoneNumber);
        Task<Staff> GetByLicenseNumberAsync(string licenseNumber);
        Task<Staff> GetByNameAsync(string name);
        Task<Staff> GetByIdsAsync(string id);
        Task<List<string>> GetAllStaffIdsAndLicenseNumbersAsync();

        Task<List<Staff>> GetByFiltersAsync(string name, string licenseNumber, string phoneNumber, string email, string specialization);

    }
}