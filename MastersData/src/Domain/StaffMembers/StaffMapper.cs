using System;
using DDDSample1.Domain.Specializations;

namespace DDDSample1.Domain.StaffMembers
{
    public class StaffMapper
    {
        public static StaffDto toDTO(Staff obj)
        {
            return new StaffDto(
                obj.Id,
                obj.FullName.fullName,
                obj.LicenseNumber.licenseNumber,
                obj.SpecializationId.Value,
                obj.Email.email,
                obj.PhoneNumber.phoneNumber,
                obj.Category.ToString(),
                obj.status.ToString()
            );
        }

        public static StaffDtoUI toDtoForUI(Staff obj, string Id, string specializatioName)
        {
            return new StaffDtoUI(
                Id,
                obj.FullName.fullName,
                obj.LicenseNumber.licenseNumber,
                specializatioName,
                obj.Email.email,
                obj.PhoneNumber.phoneNumber,
                obj.Category.ToString(),
                obj.status.ToString()
            );
        }



        public static Staff toDomain(StaffDto dto, StaffId staffId)
        {
            return new Staff(
                staffId,
                dto.FullName,
                dto.LicenseNumber,
                new SpecializationId(dto.SpecializationId),
                dto.Email,
                dto.PhoneNumber,
                dto.Category
            );
        }
    }
}
