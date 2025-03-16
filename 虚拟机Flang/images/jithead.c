struct string
{ 
	char *data;
	long long int len;
	long long int size; 
};

struct object_type
{
	struct object_type* base;
	int super_class;
	
	char type;
	
	char list[72];
	char virtual_list[72];
	char member_list[72];
	
	void* class_position;
	
	char lib_example[72];
	
	int mark;
};
struct value_type
{
	char type;
	
	long long int integer_val;
	struct string string_val;
	double double_val;
};
struct callback_type{
	char type;
	struct object_type* object_quote;
	void* method;
};
struct box_type
{
	char type;
	
	struct value_type value;
	struct object_type* object_quote;
	struct box_type* box_quote;
	struct callback_type callback_val;
};
struct stack_type
{
	char type;
	long long int integer_val;
	struct string string_val;
	double double_val;
	
	struct box_type* box_val;
	struct object_type* object_val;
	struct callback_type callback_val;
};


const char stack_box=0;
const char stack_object=1;
const char stack_integer=2;
const char stack_string=3;
const char stack_double=4;
const char stack_callback=5;

const char box_type_null=0;
const char box_type_val=1;
const char box_type_ref=2;
const char box_type_refpass=3;
const char box_type_callback=4;

const char val_type_int=0;
const char val_type_str=1;
const char val_type_dou=2;

void is_jit_print(struct stack_type** stack);
void is_jit_create(struct box_type** local_box,long long int index);
void is_jit_close(struct box_type** local_box,long long int index);

void expand_jit_msgbox(const char* text);
void expand_jit_print(const char* text);
void expand_jit_print_anyptr(void* text);
void expand_jit_print_int(int text);
void expand_jit_print_longint(long long int text);
void expand_jit_copystring(struct string* destination,const char* source);
void expand_jit_debung(struct stack_type** stack);

void method(
			struct stack_type** stack,
			struct box_type** local_box
			)
