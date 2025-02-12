using System.Threading.Tasks;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Utils;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Domain.Specializations;
using DDDSample1.Domain.StaffLoggers;
using System;
using DDDSample1.Domain.User;
using Org.BouncyCastle.Asn1.Misc;
using System.Linq;



namespace DDDSample1.Domain.StaffMembers
{
    public class StaffService : IStaffService
    {
        private readonly IUnitOfWork _unitOfWork;

        private readonly IStaffRepository _staffRepository;
        private readonly IAvailabilitySlotsRepository _availabilitySlotsRepository;

        private readonly ISpecializationRepository _specializationRepository;
        private readonly IEmailSender _emailSender;
        private readonly IStaffLoggerRepository _staffLoggerRepository;






        public StaffService(IUnitOfWork unitOfWork, IStaffRepository staffRepository, IAvailabilitySlotsRepository availabilitySlotsRepository, ISpecializationRepository specializationRepository, IEmailSender emailSender, IStaffLoggerRepository staffLoggerRepository)
        {
            _unitOfWork = unitOfWork;
            _staffRepository = staffRepository;
            _availabilitySlotsRepository = availabilitySlotsRepository;
            _specializationRepository = specializationRepository;
            _emailSender = emailSender;
            _staffLoggerRepository = staffLoggerRepository;

        }


        public async Task<StaffDto> AddAsync(CreatingStaffDto staffdto)
        {


            bool emailIsUnique = await validateEmailIsUnique(staffdto.Email);
            bool phoneNumberIsUnique = await validatePhoneNumberIsUnique(staffdto.PhoneNumber);
            if (!emailIsUnique || !phoneNumberIsUnique)
            {
                throw new BusinessRuleValidationException("Email and/or Phone Number are not unique");
            }

            await checkOSpecializationIdAsync(staffdto.SpecializationId);

            DateTime recruitmentDate = DateTime.Now;

            StaffIdGeneratorService staffIdGeneratorService = new StaffIdGeneratorService();
            Category category = Enum.Parse<Category>(staffdto.Category);
            StaffId staffId = staffIdGeneratorService.generateStaffId(category, recruitmentDate);

            var staff = new Staff(staffId, staffdto.FullName, staffdto.LicenseNumber, new SpecializationId(staffdto.SpecializationId), staffdto.Email, staffdto.PhoneNumber, category.ToString());

            await _staffRepository.AddAsync(staff);
            await _unitOfWork.CommitAsync();


            return StaffMapper.toDTO(staff);
        }

        public async Task<StaffDto> AddAsyncUi(CreatingStaffDto staffDto)
        {
            bool emailIsUnique = await validateEmailIsUnique(staffDto.Email);
            bool phoneNumberIsUnique = await validatePhoneNumberIsUnique(staffDto.PhoneNumber);
            if (!emailIsUnique || !phoneNumberIsUnique)
            {
                throw new BusinessRuleValidationException("Email and/or Phone Number are not unique");
            }

            await checkOSpecializationByNameAsync(staffDto.SpecializationId, staffDto);

            DateTime recruitmentDate = DateTime.Now;

            StaffIdGeneratorService staffIdGeneratorService = new StaffIdGeneratorService();
            Category category = Enum.Parse<Category>(staffDto.Category);
            StaffId staffId = staffIdGeneratorService.generateStaffId(category, recruitmentDate);

            var staff = new Staff(staffId, staffDto.FullName, staffDto.LicenseNumber, new SpecializationId(staffDto.SpecializationId), staffDto.Email, staffDto.PhoneNumber, category.ToString());

            await _staffRepository.AddAsync(staff);
            await _unitOfWork.CommitAsync();


            return StaffMapper.toDTO(staff);


        }


        public async Task<StaffDto> UpdateAsync(EditingStaffProfileDto dto)
        {
            var staff = await _staffRepository.GetByIdAsync(new StaffId(dto.Id));

            if (staff == null)
            {
                throw new BusinessRuleValidationException("Staff member not found");
            }
            Console.WriteLine("Erro antes do loggeer");
            var objetoLogger = LogObjectCreate(staff, LoggerTypeOfChange.Update);

            Console.WriteLine("Erro depois do loggeer");

            bool hasChanges = false;

            string email = staff.Email.email;
            await checkOSpecializationByNameForEditingAsync(dto.SpecializationId, dto);

            if (!string.IsNullOrWhiteSpace(dto.FullName) && !staff.FullName.fullName.Equals(dto.FullName))
            {
                staff.ChangeFullName(dto.FullName);
                hasChanges = true;
            }

            if (!string.IsNullOrWhiteSpace(dto.Email) && !staff.Email.email.Equals(dto.Email))
            {
                bool emailIsUnique = await validateEmailIsUnique(dto.Email);
                if (!emailIsUnique && !email.Equals(dto.Email))
                {
                    throw new BusinessRuleValidationException("Email already exists");
                }
                staff.ChangeEmail(dto.Email);
                hasChanges = true;
            }

            if (!string.IsNullOrWhiteSpace(dto.PhoneNumber) && !staff.PhoneNumber.phoneNumber.Equals(dto.PhoneNumber))
            {
                bool phoneNumberIsUnique = await validatePhoneNumberIsUnique(dto.PhoneNumber);
                if (!phoneNumberIsUnique && !staff.PhoneNumber.phoneNumber.Equals(dto.PhoneNumber))
                {
                    throw new BusinessRuleValidationException("Phone Number already exists");
                }
                staff.ChangePhoneNumber(dto.PhoneNumber);
                hasChanges = true;
            }


            if (!string.IsNullOrWhiteSpace(dto.LicenseNumber) && !staff.LicenseNumber.licenseNumber.Equals(dto.LicenseNumber))
            {
                staff.ChangeLicenseNumber(dto.LicenseNumber);
                hasChanges = true;
            }

            if(!string.IsNullOrWhiteSpace(dto.SpecializationId) && !staff.SpecializationId.Value.Equals(dto.SpecializationId))
            {
                staff.ChangeSpecializationId(dto.SpecializationId);
                hasChanges = true;
            }


            if (hasChanges)
            {
                await _staffLoggerRepository.AddAsync(objetoLogger);
            }

            await _unitOfWork.CommitAsync();


            if (dto.Email != null || dto.PhoneNumber != null)
            {
                //isto so vai funcionar se usares um email valido
                await _emailSender.SendEmailAsync("Your profile has been updated. If you are not aware of this this change, please contact support immediately.", email, "Profile Update Notification");
            }

            return StaffMapper.toDTO(staff);



        }

        public async Task<StaffDto> DeleteAsync(StaffId id)
        {
            var staff = await _staffRepository.GetByIdAsync(id);

            if (staff == null)
            {
                throw new BusinessRuleValidationException("Staff member not found");
            }

            if (staff.status == StaffStatus.Inactive)
            {
                throw new BusinessRuleValidationException("Staff member is already inactive");
            }

            var objetoLogger = LogObjectCreate(staff, LoggerTypeOfChange.Delete);
            await _staffLoggerRepository.AddAsync(objetoLogger);

            staff.Deactivate();

            await _unitOfWork.CommitAsync();


            return StaffMapper.toDTO(staff);
        }


        public async Task<StaffDto> GetByIdAsync(StaffId id)
        {

            var staff = await _staffRepository.GetByIdAsync(id);

            return staff == null ? null : StaffMapper.toDTO(staff);

        }

        public async Task<StaffDtoUI> GetByIdForUIAsync(StaffId id)
        {

            var staff = await _staffRepository.GetByIdAsync(id);
            var specializationName = _specializationRepository.GetByIdAsync(staff.SpecializationId).Result.Name;
            var idValue = id.AsString();


            return staff == null ? null : StaffMapper.toDtoForUI(staff, idValue, specializationName);

        }





        public async Task<Specialization> checkOSpecializationIdAsync(string specializationId)
        {

            try
            {
                var id = new SpecializationId(specializationId);
                Console.WriteLine("ID ->: " + id);
                var spec = await this._specializationRepository.GetByIdAsync(id);

                if (spec == null)
                {
                    throw new BusinessRuleValidationException("Specialization does not exist");
                }

                return spec;
            }
            catch (Exception e)
            {
                throw new BusinessRuleValidationException("Specialization does not exist");
            }
        }

        public async Task<Specialization> checkOSpecializationByNameAsync(string specialization, CreatingStaffDto staff)
        {

            try
            {
                var spec = await this._specializationRepository.GetByNameAsync(specialization);

                if (spec == null)
                {
                    throw new BusinessRuleValidationException("Specialization not found");
                }
                staff.SpecializationId = spec.Id.AsString();
                return spec;
            }
            catch (Exception e)
            {
                throw new BusinessRuleValidationException("Specialization not Found");
            }
        }

        public async Task<Specialization> checkOSpecializationByNameForEditingAsync(string specialization, EditingStaffProfileDto staff)
        {

            try
            {
                var spec = await this._specializationRepository.GetByNameAsync(specialization);

                if (spec == null)
                {
                    throw new BusinessRuleValidationException("Specialization not found");
                }
                staff.SpecializationId = spec.Id.AsString();
                return spec;
            }
            catch (Exception e)
            {
                throw new BusinessRuleValidationException("Specialization not Found");
            }
        }



        private async Task<bool> validateEmailIsUnique(string email)
        {
            var existingStaff = await _staffRepository.GetByEmailAsync(email);
            if (existingStaff != null)
            {
                return false;
            }
            return true;
        }

        private async Task<bool> validatePhoneNumberIsUnique(string phoneNumber)
        {
            var existingStaff = await _staffRepository.GetByPhoneNumberAsync(phoneNumber);
            if (existingStaff != null)
            {
                return false;
            }
            return true;
        }



        public async Task<List<StaffDto>> GetAllAsync()
        {
            var list = await _staffRepository.GetAllAsync();

            List<StaffDto> listDto = list.ConvertAll<StaffDto>(sta => StaffMapper.toDTO(sta));

            return listDto;
        }

        private StaffLogger LogObjectCreate(Staff staff, LoggerTypeOfChange typeOfChange)
        {
            return new StaffLogger(
                     staff.Id.AsString(),
                     staff.FullName.fullName,
                     staff.SpecializationId.Value,
                     staff.Email.email,
                     staff.PhoneNumber.phoneNumber,
                     staff.Category.ToString(),
                     typeOfChange.ToString(),
                     DateTime.UtcNow);
        }

        public async Task<List<ViewStaffDto>> SearchAsync(StaffFilterDto dto)
        {

            var staff = new List<Staff>();

            if (string.IsNullOrWhiteSpace(dto.Name) && string.IsNullOrWhiteSpace(dto.LicenseNumber) && string.IsNullOrWhiteSpace(dto.Email) && string.IsNullOrWhiteSpace(dto.PhoneNumber) && string.IsNullOrWhiteSpace(dto.Specialization))
            {
                staff = await _staffRepository.GetAllAsync();
            }

            var specializationId = "";
            if (!string.IsNullOrEmpty(dto.Specialization))
            {
                var specialization = await _specializationRepository.GetByNameAsync(dto.Specialization);
                specializationId = specialization.Id.Value;
            }

            staff = await _staffRepository.GetByFiltersAsync(dto.Name, dto.LicenseNumber, dto.Email, dto.PhoneNumber, specializationId);


            List<ViewStaffDto> listDto = staff.ConvertAll<ViewStaffDto>(sta => new ViewStaffDto
            {
                Id = sta.Id.Value,
                FullName = sta.FullName.fullName,
                LicenseNumber = sta.LicenseNumber.licenseNumber,
                Email = sta.Email.email,
                PhoneNumber = sta.PhoneNumber.phoneNumber,
                SpecializationId = sta.SpecializationId.Value,
                Category = sta.Category.ToString(),
                Status = sta.status.ToString()
            });

            List<ViewStaffDto> result = new List<ViewStaffDto>();
            if (listDto != null)
            {

                result = TransformStaffsForUi(listDto);

            }


            return result;

        }


        public async Task<List<StaffDtoUI>> GetAllForUiAsync()
        {
            List<Staff> staff = await this._staffRepository.GetAllAsync();
            List<StaffDtoUI> staffDtos = new List<StaffDtoUI>();
            foreach (Staff sta in staff)
            {
                var specializationName = _specializationRepository.GetByIdAsync(sta.SpecializationId).Result.Name;
                var idValue = _staffRepository.GetByIdAsync(sta.Id).Result.Id.AsString();
                staffDtos.Add(StaffMapper.toDtoForUI(sta, idValue, specializationName));
            }
            return staffDtos;
        }


        public List<ViewStaffDto> TransformStaffsForUi(List<ViewStaffDto> staffs)
        {
            List<ViewStaffDto> staffDtos = new List<ViewStaffDto>();

            foreach (ViewStaffDto staff in staffs)
            {

                var specializationName = _specializationRepository.GetByIdAsync(new SpecializationId(staff.SpecializationId)).Result.Name;
                staffDtos.Add(new ViewStaffDto
                {
                    Id = staff.Id,
                    FullName = staff.FullName,
                    LicenseNumber = staff.LicenseNumber,
                    SpecializationId = specializationName,
                    Email = staff.Email,
                    PhoneNumber = staff.PhoneNumber,
                    Category = staff.Category,
                    Status = staff.Status
                });
            }

            return staffDtos;


        }





    }

}
