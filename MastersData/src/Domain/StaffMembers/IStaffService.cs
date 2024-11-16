
using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.StaffMembers;


namespace DDDSample1.Domain.StaffMembers
{

    public interface IStaffService
    {
        Task<StaffDto> AddAsync(CreatingStaffDto staffDto);
        //Task<StaffDto> AddAsync(CreatingStaffDto staffDto);
        Task<StaffDto> UpdateAsync(EditingStaffProfileDto dto);
        Task<StaffDto> DeleteAsync(StaffId id);
        Task<List<StaffDto>> GetAllAsync();
        Task<StaffDto> GetByIdAsync(StaffId id);
        Task<List<ViewStaffDto>> SearchAsync(StaffFilterDto staffFilterDto);




    }


}