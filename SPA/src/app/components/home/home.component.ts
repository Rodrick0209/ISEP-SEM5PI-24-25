import { Component, OnInit } from '@angular/core';
import { AuthService } from '../../services/auth.service';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';

@Component({
  selector: 'app-home',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './home.component.html',
  styleUrl: './home.component.css'
})
export class HomeComponent implements OnInit {
  isLoggedIn: boolean = false;

  constructor(private authService: AuthService, private router: Router){ }

  ngOnInit(): void {
    this.authService.isLoggedIn$.subscribe(status => {
      this.isLoggedIn = status;
    });
  }

  extractRole(): any {
    return this.authService.extractRoleFromToken();
  }

  extractEmail(): any {
    return this.authService.extractEmailFromToken();
  }

  isAdmin(): boolean {
    const role = this.extractRole();
    return role === 'admin';
  }

  isDoctor(): boolean {
    const role = this.extractRole();
    return role === 'doctor';
  }

  isPatient(): boolean {
    const role = this.extractRole();
    return role === 'patient';
  }


  scheduleSurgeries() {
    this.router.navigate(['/planning']);
  }

  managePatients() {
    this.router.navigate(['/patients']);
  }

  manageOperationTypes() {
    this.router.navigate(['/operationType']);
  }

  manageStaffs() {
    this.router.navigate(['/staffs']);
  }

  manageSpecializations() {
    this.router.navigate(['/specializations'])
  }

  addRoomTypes(){
    this.router.navigate(['/room-types/add'])
  }

  manageOperationRequests(){
    this.router.navigate(['/operationRequests'])
  }
  manageAppointments(){
    this.router.navigate(['/appointments'])
  }

  manageMedicalRecord(){
    this.router.navigate(['/medical-record'])
  }

  viewProfile(){
    const email = this.extractEmail();
    this.router.navigate(['/profile', `${email}`])
  }

  downloadPatientRecord(){
    const email = this.extractEmail();
    this.router.navigate(['/download-medical-history', `${email}`])
  }

  login(){
    this.router.navigate(["/login"])
  }

  signUp(){
    this.router.navigate(['/register'])
  }

  about(){
    this.router.navigate(['/about'])
  }

  contact(){
    this.router.navigate(['/contact'])
  }


  manageAllergiesCatalog(){
    this.router.navigate(['/allergiesCatalog'])
  }

  manageMedicalConditions(){
    this.router.navigate(['/medicalConditions'])
  }

  viewOperationRooms(){
    this.router.navigate(['/view'])
  }

}
