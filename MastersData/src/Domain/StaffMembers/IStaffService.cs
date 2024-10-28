
using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.StaffMembers;


namespace DDDSample1.Domain.StaffMembers
{

    public interface IStaffService
    {
        Task<StaffDto> AddAsync(StaffDto staffDto);
        Task<StaffDto> UpdateAsync(EditingStaffProfileDto dto);
        Task<Staff> DeleteAsync(StaffId id);
        Task<List<Staff>> GetAllAsync();
        Task<Staff> GetByIdAsync(StaffId id);
        Task<List<ViewStaffDto>> SearchAsync(StaffFilterDto staffFilterDto);




    }


}