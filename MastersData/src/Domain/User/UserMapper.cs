using System;

namespace DDDSample1.Domain.User
{
    public  class UserMapper
    {
        public static UserDTO ToDto(User obj)
        {
            return new UserDTO(obj.Id.AsGuid(),obj.role.role,obj.email.email,obj.password.password);
        }

        public static User ToDomain(UserDTO dto)
        {
            var User = new User(dto.email, dto.role);
            // sitio errado para fazer isto tem de ser mudado
            PasswordHasher passwordHasher = new PasswordHasher();
            string password = passwordHasher.HashPassword(dto.password);
            System.Console.WriteLine(password);
            User.SetPassword(password); 
            return User;
        }

    }
}