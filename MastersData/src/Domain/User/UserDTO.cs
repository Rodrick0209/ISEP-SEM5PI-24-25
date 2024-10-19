using System;



namespace DDDSample1.Domain.User
{
    public class UserDTO
    {
        public Guid Id { get; set; }
        public string role { get; set; }
        public string email { get; set; }
        public string password { get; set; }

        public UserDTO(Guid Id, string role, string email,string password)
        {
            this.Id = Id;
            this.role = role;
            this.email = email;
            this.password = password;
        }


    } 


}