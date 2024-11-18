import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ActivatedRoute, Router } from '@angular/router';
import { of } from 'rxjs';
import { DeleteUserComponent } from './delete-user.component';
import { UserService } from '../../services/user.service';
import { MarkXComponent } from '../template/mark-x/mark-x.component';
import { CommonModule } from '@angular/common';

describe('DeleteUserComponent', () => {
  let component: DeleteUserComponent;
  let fixture: ComponentFixture<DeleteUserComponent>;
  let userServiceSpy: jasmine.SpyObj<UserService>;
  let routerSpy: jasmine.SpyObj<Router>;

  beforeEach(async () => {
    userServiceSpy = jasmine.createSpyObj('UserService', ['delete']);
    routerSpy = jasmine.createSpyObj('Router', ['navigate']);

    await TestBed.configureTestingModule({
      imports: [DeleteUserComponent, CommonModule, MarkXComponent],
      providers: [
        { provide: UserService, useValue: userServiceSpy },
        { provide: Router, useValue: routerSpy },
        { 
          provide: ActivatedRoute,
          useValue: {
            snapshot: {
              paramMap: {
                get: (email: string) => 'test@example.com'
              }
            }
          }
        }
      ]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(DeleteUserComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize email from route parameters', () => {
    expect(component.email).toBe('test@example.com');
  });

  it('should toggle isConfirmed when onMarkClick is called', () => {
    expect(component.isConfirmed).toBeFalse();
    component.onMarkClick();
    expect(component.isConfirmed).toBeTrue();
    component.onMarkClick();
    expect(component.isConfirmed).toBeFalse();
  });

  it('should set errorMessage if delete is called without confirmation', () => {
    component.onDelete();
    expect(component.errorMessage).toBe('Please confirm by clicking in the mark.');
  });

  it('should call userService.delete and set successMessage on successful delete', () => {
    component.isConfirmed = true;
    userServiceSpy.delete.and.returnValue(of({}));
    component.onDelete();
    expect(userServiceSpy.delete).toHaveBeenCalledWith('test@example.com');
    expect(component.successMessage).toBe('An email confirmation was sent to your inbox. Please confirm the deletion of your account and profile by clicking in the link.');
  });

  it('should set errorMessage on delete failure', () => {
    component.isConfirmed = true;
    userServiceSpy.delete.and.returnValue(of({ error: 'Failed to delete your account.' }));
    component.onDelete();
    expect(userServiceSpy.delete).toHaveBeenCalledWith('test@example.com');
    expect(component.errorMessage).toBe('');
  });

  it('should navigate to profile on cancel', () => {
    component.onCancel();
    expect(routerSpy.navigate).toHaveBeenCalledWith(['/profile', 'test@example.com']);
  });
});